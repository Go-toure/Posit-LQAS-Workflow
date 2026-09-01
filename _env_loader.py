"""
_env_loader.py
Tiny .env-style loader for the LQAS pipeline's Python scripts.

Same pattern already used by im_workflow and the AFRO-SIA scope
project: reads simple KEY=VALUE lines from config/secrets.env
(git-ignored -- see config/secrets.env.example) and loads them into
os.environ, WITHOUT overwriting a variable that is already set in the
real environment (so `setx` / `export` always wins over the file,
matching normal .env conventions).
"""

import os


def load_secrets_env(base_dir):
    """Load <base_dir>/config/secrets.env into os.environ, if it exists."""
    path = os.path.join(str(base_dir), "config", "secrets.env")
    if not os.path.exists(path):
        return
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#") or "=" not in line:
                continue
            key, _, value = line.partition("=")
            os.environ.setdefault(key.strip(), value.strip())
