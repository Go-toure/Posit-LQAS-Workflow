#!/usr/bin/env python3
# ============================================================
# REFRESH LQAS LOOKUP (campaign calendar)
#
# Fills in the "missing piece" needed to fully automate this pipeline:
# data/lookup/lqas_lookup.xlsx used to be a file someone downloaded by
# hand from the WHO AFRO "Status of campaign preparedness" Power BI
# dashboard and dropped into this folder -- easy to forget, easy to
# go stale. This script refreshes it automatically, the same way the
# equivalent files are now refreshed in the im_workflow and
# AFRO-SIA-Dashboard-input projects: by calling poliogo.afro.who.int's
# own REST API directly (GPEI_API_TOKEN), instead of a manual export.
#
# R/02_process_lqas.r and R/03_clean_geonames.R only ever read these
# columns from lqas_lookup.xlsx: Country, OBR Name, Vaccines,
# Round Number, Round Start Date (confirmed by grepping both scripts'
# read_excel() calls) -- so that's exactly what this script produces,
# nothing more.
#
# Only rows whose computed status is "Finished" or "In Progress" are
# kept (derived from each round's started_at/ended_at against today's
# date) -- this mirrors the "Round / Sub-activity status" slicer
# selection (Finished + In Progress, excluding "To start within 28
# days") that the old manual Power BI export used, and matches the
# same filter already used by im_workflow's
# scripts/refresh_preparedness_lookup.py for this exact same source.
# LQAS is a post-campaign survey methodology, so rounds that haven't
# started yet wouldn't have real LQAS data associated anyway. Only
# campaigns whose campaign_types include "polio" are included.
#
# SAFETY: the fetched data is validated before it's allowed to replace
# data/lookup/lqas_lookup.xlsx --
#   - must have the expected columns
#   - Round Status must contain ONLY "Finished"/"In Progress"
#   - row count must not have collapsed to a small fraction of the
#     previous file's row count (catches a partial/broken fetch)
# If any check fails, the existing lqas_lookup.xlsx is left untouched,
# a clear error is printed, and the script exits non-zero. The
# previous file is also backed up (data/lookup/backups/) before every
# successful replacement.
#
# Never prints the token itself anywhere.
#
# Setup (one-time, on the machine that runs this):
#   pip install requests pandas openpyxl
#
# Credentials (config/secrets.env, see config/secrets.env.example):
#   GPEI_API_TOKEN=<your token from https://poliogo.afro.who.int/dashboard/api/apitoken>
#
# Usage (run from the project root, same as fetch_ona_data.py):
#   python refresh_lqas_lookup.py
#   python refresh_lqas_lookup.py --debug   (saves a fetch summary to data/lookup/refresh_debug/)
# ============================================================

import argparse
import os
import shutil
import sys
from datetime import date, datetime
from pathlib import Path

GPEI_API_BASE_URL = "https://poliogo.afro.who.int"

# Columns lqas_lookup.xlsx is expected to have (matches exactly what
# R/02_process_lqas.r and R/03_clean_geonames.R read via read_excel()).
REQUIRED_COLUMNS = ["Country", "OBR Name", "Vaccines", "Round Number", "Round Start Date"]
STATUS_COLUMN = "Round Status"
ALLOWED_STATUSES = {"Finished", "In Progress"}

# Below this fraction of the PREVIOUS file's row count, treat the new
# fetch as a probable partial/broken result rather than real data.
MIN_ROW_FRACTION_OF_PREVIOUS = 0.5
MIN_ABSOLUTE_ROWS = 20


def log(msg):
    print(f"[refresh_lqas_lookup] {msg}", flush=True)


def fetch_all_campaigns(headers, fieldset, page_limit=100):
    """Paginate GET /api/polio/campaigns/?fieldset=... and return every
    campaign row across all pages, keyed by campaign id."""
    import requests

    by_id = {}
    page = 1
    while True:
        r = requests.get(
            f"{GPEI_API_BASE_URL}/api/polio/campaigns/",
            headers=headers,
            params={"fieldset": fieldset, "limit": page_limit, "page": page,
                    "campaign_category": "all", "show_test": "true",
                    "is_embedded": "false", "enabled": "true"},
            timeout=60,
        )
        r.raise_for_status()
        data = r.json()
        for c in data.get("campaigns", []):
            by_id[c["id"]] = c
        log(f"  fieldset={fieldset}: page {page}/{data.get('pages')} -- {len(by_id)} campaigns so far")
        if not data.get("has_next"):
            break
        page += 1
    return by_id


def is_polio_campaign(list_entry):
    return any(ct.get("slug") == "polio" for ct in (list_entry.get("campaign_types") or []))


def compute_round_status(round_obj, today):
    if round_obj.get("is_planned"):
        return None
    started = round_obj.get("started_at")
    ended = round_obj.get("ended_at")
    if not started:
        return None
    started_d = date.fromisoformat(started)
    if started_d > today:
        return None
    if ended:
        ended_d = date.fromisoformat(ended)
        return "Finished" if ended_d < today else "In Progress"
    return "In Progress"


def fetch_lookup_rows(headers):
    today = date.today()
    list_by_id = fetch_all_campaigns(headers, "list")
    calendar_by_id = fetch_all_campaigns(headers, "calendar")
    polio_ids = [cid for cid, c in list_by_id.items() if is_polio_campaign(c)]
    log(f"{len(polio_ids)} of {len(list_by_id)} campaigns are Polio-type.")

    rows = []
    for cid in polio_ids:
        list_entry = list_by_id[cid]
        cal_entry = calendar_by_id.get(cid)
        if not cal_entry:
            continue
        obr_name = list_entry.get("obr_name")
        country = list_entry.get("top_level_org_unit_name")
        for rnd in cal_entry.get("rounds", []):
            number = rnd.get("number")
            started_at = rnd.get("started_at")
            vaccine_names = rnd.get("vaccine_names") or ""
            status = compute_round_status(rnd, today)
            if status and started_at:
                rows.append({
                    "Round Number": f"Round {number}",
                    "OBR Name": obr_name,
                    "Vaccines": vaccine_names,
                    "Country": country,
                    "Round Start Date": started_at,
                    "Round Status": status,
                })
    return rows


def run(base_dir: Path, debug: bool) -> bool:
    sys.path.insert(0, str(base_dir))
    from _env_loader import load_secrets_env
    load_secrets_env(str(base_dir))

    token = os.environ.get("GPEI_API_TOKEN", "").strip()
    if not token:
        log("FAILED: GPEI_API_TOKEN is not set. Get it from "
            "https://poliogo.afro.who.int/dashboard/api/apitoken and add it to "
            "config/secrets.env as GPEI_API_TOKEN=... (treat it like a password -- "
            "never paste it anywhere else). Keeping the existing lqas_lookup.xlsx.")
        return False
    headers = {"Authorization": f"Bearer {token}", "Accept": "application/json"}

    lookup_dir = base_dir / "data" / "lookup"
    lookup_path = lookup_dir / "lqas_lookup.xlsx"
    backup_dir = lookup_dir / "backups"
    debug_dir = (lookup_dir / "refresh_debug") if debug else None
    lookup_dir.mkdir(parents=True, exist_ok=True)

    try:
        rows = fetch_lookup_rows(headers)
    except Exception as e:
        log(f"FAILED: could not fetch campaign data from the GPEI API ({e}). "
            f"Keeping the existing lqas_lookup.xlsx.")
        return False

    if debug_dir is not None:
        try:
            debug_dir.mkdir(parents=True, exist_ok=True)
            summary_path = debug_dir / "gpei_api_fetch_summary.txt"
            summary_path.write_text(
                f"Fetched at: {datetime.now().isoformat()}\n"
                f"Rows fetched: {len(rows)}\n"
                f"Sample row: {rows[0] if rows else '(none)'}\n",
                encoding="utf-8",
            )
            log(f"Saved fetch summary: {summary_path}")
        except Exception as e:
            log(f"Could not save debug summary: {e}")

    import pandas as pd

    tmp_path = lookup_dir / f"_lqas_lookup_download_{datetime.now().strftime('%Y%m%d_%H%M%S')}.xlsx"
    try:
        pd.DataFrame(rows).to_excel(tmp_path, index=False)
    except PermissionError as e:
        log(f"FAILED: could not write a temp download file ({e}). Keeping the existing lqas_lookup.xlsx.")
        return False

    return validate_and_install(tmp_path, lookup_path, backup_dir)


def validate_and_install(new_path: Path, lookup_path: Path, backup_dir: Path) -> bool:
    import pandas as pd

    if not new_path.exists() or new_path.stat().st_size < 1000:
        log(f"FAILED: downloaded file missing or too small ({new_path}). Keeping the existing lqas_lookup.xlsx.")
        return False

    try:
        df = pd.read_excel(new_path)
    except Exception as e:
        log(f"FAILED: downloaded file is not a readable Excel file ({e}). Keeping the existing lqas_lookup.xlsx.")
        return False

    missing_cols = [c for c in REQUIRED_COLUMNS if c not in df.columns]
    if missing_cols:
        log(f"FAILED: downloaded file is missing expected columns: {missing_cols}. "
            f"Got columns: {list(df.columns)}. Keeping the existing lqas_lookup.xlsx.")
        return False

    if STATUS_COLUMN in df.columns:
        bad_statuses = set(df[STATUS_COLUMN].dropna().unique()) - ALLOWED_STATUSES
        if bad_statuses:
            log(f"FAILED: exported rows include statuses other than {ALLOWED_STATUSES}: {bad_statuses}. "
                f"This means the Finished/In Progress filter wasn't actually applied. "
                f"Keeping the existing lqas_lookup.xlsx.")
            return False
    else:
        log(f"WARNING: '{STATUS_COLUMN}' column not present in the export -- can't verify the filter was applied.")

    new_rows = len(df)
    if new_rows < MIN_ABSOLUTE_ROWS:
        log(f"FAILED: only {new_rows} rows in the download (expected at least {MIN_ABSOLUTE_ROWS}). "
            f"Keeping the existing lqas_lookup.xlsx.")
        return False

    if lookup_path.exists():
        try:
            prev_rows = len(pd.read_excel(lookup_path))
            if prev_rows > 0 and new_rows < prev_rows * MIN_ROW_FRACTION_OF_PREVIOUS:
                log(f"FAILED: new file has {new_rows} rows, previous had {prev_rows} "
                    f"(< {int(MIN_ROW_FRACTION_OF_PREVIOUS * 100)}% of previous). "
                    f"Looks like a partial/broken export. Keeping the existing lqas_lookup.xlsx.")
                return False
        except Exception as e:
            log(f"Could not read the previous lqas_lookup.xlsx to compare row counts ({e}); proceeding anyway.")

    # All checks passed -- back up the previous file, then install the new one.
    if lookup_path.exists():
        backup_dir.mkdir(parents=True, exist_ok=True)
        backup_path = backup_dir / f"lqas_lookup_{datetime.now().strftime('%Y%m%d_%H%M%S')}.xlsx"
        shutil.copy2(lookup_path, backup_path)
        log(f"Backed up previous lqas_lookup.xlsx -> {backup_path}")

    shutil.move(str(new_path), str(lookup_path))
    log(f"SUCCESS: lqas_lookup.xlsx refreshed ({new_rows} rows, statuses: "
        f"{sorted(set(df[STATUS_COLUMN].dropna().unique())) if STATUS_COLUMN in df.columns else 'unknown'}).")
    return True


def main():
    parser = argparse.ArgumentParser(description="Refresh data/lookup/lqas_lookup.xlsx from the GPEI API (poliogo.afro.who.int), using GPEI_API_TOKEN.")
    parser.add_argument("--base-dir", default=os.getcwd(),
                         help="Project root directory (default: current working directory, matching how fetch_ona_data.py is run).")
    parser.add_argument("--debug", action="store_true", help="Save a fetch summary to data/lookup/refresh_debug/.")
    args = parser.parse_args()

    ok = run(Path(args.base_dir), debug=args.debug)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
