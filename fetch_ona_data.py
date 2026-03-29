#!/usr/bin/env python3
"""
LQAS Data Fetcher - Optimized for Large Datasets
Saves data as Parquet (compressed, columnar) - readable by both Python and R
"""

import os
import sys
import json
import time
import requests
import pandas as pd
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Any
import argparse

# Configuration
ONA_API_TOKEN = os.environ.get("ONA_API_TOKEN", "48c90cee2702db978600f784a07738592fa77d60")
BASE_URL = "https://api.whonghub.org/api/v1/data"

# Output directory for Parquet files
OUTPUT_DIR = Path("data/raw")

def flatten_dict(data: Dict, parent_key: str = "", sep: str = "/") -> Dict:
    """Flatten nested dictionary for ONA data structure"""
    flattened = {}
    for key, value in data.items():
        # Construct the new key, ensuring that "Count_HH" is not duplicated
        if parent_key and not parent_key.endswith("Count_HH"):
            new_key = f"{parent_key}{sep}{key}"
        else:
            new_key = f"{parent_key}{key}" if parent_key else key
        
        if isinstance(value, dict):
            flattened.update(flatten_dict(value, new_key, sep=sep))
        elif isinstance(value, list):
            for i, item in enumerate(value, 1):
                if isinstance(item, dict):
                    flattened.update(flatten_dict(item, f"{new_key}[{i}]", sep=sep))
                else:
                    flattened[f"{new_key}[{i}]"] = str(item) if item is not None else ""
        else:
            flattened[new_key] = str(value) if value is not None else ""
    return flattened

def fetch_page(form_id: int, page: int, page_size: int = 10000) -> List[Dict]:
    """Fetch a single page of data"""
    headers = {"Authorization": f"Token {ONA_API_TOKEN}"}
    params = {"page": page, "page_size": page_size}
    
    try:
        response = requests.get(
            f"{BASE_URL}/{form_id}.json",
            params=params,
            headers=headers,
            timeout=120
        )
        
        if response.status_code == 200:
            return response.json()
        elif response.status_code == 429:
            print(f"  ⏳ Rate limited, waiting...")
            time.sleep(30)
            return fetch_page(form_id, page, page_size)
        elif response.status_code in [401, 403]:
            print(f"  ❌ Authentication failed for form {form_id}")
            return []
        elif response.status_code == 404:
            print(f"  ⚠️ Form {form_id} not found")
            return []
        else:
            print(f"  ❌ Error {response.status_code} for form {form_id}")
            return []
            
    except requests.exceptions.Timeout:
        print(f"  ⏰ Timeout fetching page {page} for form {form_id}")
        return []
    except Exception as e:
        print(f"  ❌ Exception: {e}")
        return []

def fetch_all_data(form_id: int) -> List[Dict]:
    """Fetch all data for a specific form"""
    all_data = []
    page = 1
    page_size = 10000
    
    print(f"📡 Fetching form {form_id}...")
    
    while True:
        print(f"  Page {page}...", end=" ")
        data = fetch_page(form_id, page, page_size)
        
        if not data:
            print("No data")
            break
        
        # Flatten each record
        flattened_data = [flatten_dict(datum) for datum in data]
        
        print(f"{len(data)} records")
        all_data.extend(flattened_data)
        
        if len(data) < page_size:
            break
            
        page += 1
        time.sleep(0.5)
    
    print(f"✅ Form {form_id}: {len(all_data)} total records")
    return all_data

def save_to_parquet(data: List[Dict], form_id: int) -> bool:
    """Save data to Parquet file (readable by both Python and R)"""
    if not data:
        print(f"⚠️ No data to save for form {form_id}")
        return False
    
    try:
        # Convert to DataFrame
        df = pd.DataFrame(data).fillna("").astype(str)
        
        # Extract GPS components if present
        if "GPS_hh" in df.columns:
            gps_parts = df["GPS_hh"].str.split(" ", expand=True)
            if len(gps_parts.columns) >= 3:
                df["_GPS_hh_latitude"] = gps_parts[0]
                df["_GPS_hh_longitude"] = gps_parts[1]
                df["_GPS_hh_altitude"] = gps_parts[2]
                if len(gps_parts.columns) >= 4:
                    df["_GPS_hh_precision"] = gps_parts[3]
        
        if "GPS_hh_end" in df.columns:
            gps_end_parts = df["GPS_hh_end"].str.split(" ", expand=True)
            if len(gps_end_parts.columns) >= 3:
                df["_GPS_hh_end_latitude"] = gps_end_parts[0]
                df["_GPS_hh_end_longitude"] = gps_end_parts[1]
                df["_GPS_hh_end_altitude"] = gps_end_parts[2]
                if len(gps_end_parts.columns) >= 4:
                    df["_GPS_hh_end_precision"] = gps_end_parts[3]
        
        # Create output directory
        OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
        
        # Save as Parquet (compressed, readable by R via arrow::read_parquet)
        output_path = OUTPUT_DIR / f"{form_id}.parquet"
        
        # Use pyarrow (best compatibility with R)
        try:
            df.to_parquet(
                output_path, 
                engine='pyarrow', 
                compression='snappy', 
                index=False
            )
            print(f"✅ Saved {len(df)} rows to {output_path}")
        except ImportError:
            # Fallback to fastparquet if pyarrow not available
            try:
                df.to_parquet(
                    output_path, 
                    engine='fastparquet', 
                    compression='snappy', 
                    index=False
                )
                print(f"✅ Saved {len(df)} rows to {output_path} (fastparquet)")
            except ImportError:
                print(f"❌ No parquet engine available. Install: pip install pyarrow")
                return False
        
        file_size_mb = output_path.stat().st_size / (1024 * 1024)
        csv_size_estimate = df.memory_usage(deep=True).sum() / (1024 * 1024)
        
        print(f"   📦 Parquet size: {file_size_mb:.2f} MB")
        print(f"   📄 CSV would be ~{csv_size_estimate:.2f} MB")
        print(f"   💾 Compression ratio: {csv_size_estimate/file_size_mb:.1f}x")
        print(f"   🔧 R can read with: arrow::read_parquet('{output_path}')")
        
        # Save metadata for tracking
        metadata = {
            'form_id': form_id,
            'records': len(df),
            'columns': len(df.columns),
            'parquet_size_mb': round(file_size_mb, 2),
            'estimated_csv_size_mb': round(csv_size_estimate, 2),
            'compression_ratio': round(csv_size_estimate / file_size_mb, 1),
            'last_fetch': datetime.now().isoformat(),
            'shape': f"{df.shape[0]}x{df.shape[1]}",
            'format': 'parquet',
            'engine': 'pyarrow'
        }
        
        metadata_path = OUTPUT_DIR / f"{form_id}_metadata.json"
        with open(metadata_path, 'w') as f:
            json.dump(metadata, f, indent=2)
        
        return True
        
    except Exception as e:
        print(f"❌ Failed to save form {form_id}: {e}")
        import traceback
        traceback.print_exc()
        return False

def load_form_ids(config_path: str = None) -> List[int]:
    """Load form IDs from config file or return default list"""
    if config_path and Path(config_path).exists():
        try:
            import yaml
            with open(config_path, 'r') as f:
                config = yaml.safe_load(f)
                # Try different possible config structures
                form_ids = config.get('ona', {}).get('forms', {}).get('all', [])
                if not form_ids:
                    form_ids = config.get('ona', {}).get('form_ids', [])
                if not form_ids:
                    form_ids = config.get('form_ids', [])
                return form_ids
        except Exception as e:
            print(f"⚠️ Error loading config: {e}")
    
    # Default ALL form IDs from your original code
    return [
        4500, 8588, 10271, 5203, 4388, 8834, 4450, 9601, 4436, 4431,
        4999, 4481, 8094, 6837, 3583, 5299, 7589, 6420, 5889, 8281,
        7602, 4987, 6429, 7623, 15782, 5214, 9794, 6350, 6104, 5777,
        7614, 4419, 6769, 4351, 7983
    ]

def test_api_connection():
    """Test API connection and authentication"""
    headers = {"Authorization": f"Token {ONA_API_TOKEN}"}
    try:
        # Try to fetch user info to test token
        response = requests.get("https://api.whonghub.org/api/v1/user.json", headers=headers, timeout=30)
        if response.status_code == 200:
            print("✅ API connection successful")
            return True
        else:
            print(f"❌ API connection failed: {response.status_code}")
            return False
    except Exception as e:
        print(f"❌ API connection error: {e}")
        return False

def main():
    parser = argparse.ArgumentParser(description='Fetch LQAS data from ONA and save as Parquet')
    parser.add_argument('--config', help='Config file path')
    parser.add_argument('--force-full', action='store_true', help='Force full fetch (overwrite existing)')
    parser.add_argument('--form-ids', help='Comma-separated form IDs to fetch')
    parser.add_argument('--test', action='store_true', help='Test API connection only')
    args = parser.parse_args()
    
    if args.test:
        test_api_connection()
        return
    
    # Determine which forms to fetch
    if args.form_ids:
        form_ids = [int(f.strip()) for f in args.form_ids.split(',')]
    else:
        form_ids = load_form_ids(args.config)
    
    print("=" * 60)
    print("🚀 LQAS Data Fetcher (Parquet Format)")
    print("=" * 60)
    print(f"📊 Forms to fetch: {len(form_ids)}")
    print(f"📁 Output directory: {OUTPUT_DIR.absolute()}")
    print(f"📄 Format: Parquet (readable by Python & R)")
    print("=" * 60)
    
    # Test API first
    if not test_api_connection():
        print("❌ Cannot proceed - API connection failed")
        sys.exit(1)
    
    # Track results
    results = []
    total_records = 0
    total_size_mb = 0
    
    for i, form_id in enumerate(form_ids, 1):
        print(f"\n--- Processing form {form_id} ({i}/{len(form_ids)}) ---")
        
        # Check if file already exists and not forcing full fetch
        output_path = OUTPUT_DIR / f"{form_id}.parquet"
        if output_path.exists() and not args.force_full:
            file_size = output_path.stat().st_size / (1024 * 1024)
            print(f"⏭️  File already exists: {output_path} ({file_size:.2f} MB)")
            # Load metadata to show info
            metadata_path = OUTPUT_DIR / f"{form_id}_metadata.json"
            if metadata_path.exists():
                with open(metadata_path, 'r') as f:
                    meta = json.load(f)
                    print(f"   📊 Records: {meta.get('records', 'N/A')}")
                    print(f"   📅 Last fetch: {meta.get('last_fetch', 'N/A')}")
            results.append({'form_id': form_id, 'status': 'skipped', 'records': 0})
            continue
        
        # Fetch data
        data = fetch_all_data(form_id)
        if data:
            success = save_to_parquet(data, form_id)
            if success:
                file_size = output_path.stat().st_size / (1024 * 1024)
                results.append({'form_id': form_id, 'status': 'success', 'records': len(data)})
                total_records += len(data)
                total_size_mb += file_size
            else:
                results.append({'form_id': form_id, 'status': 'failed', 'records': 0})
        else:
            results.append({'form_id': form_id, 'status': 'failed', 'records': 0})
    
    # Summary
    print("\n" + "=" * 60)
    print("📊 FETCH SUMMARY")
    print("=" * 60)
    success_count = sum(1 for r in results if r['status'] == 'success')
    skipped_count = sum(1 for r in results if r['status'] == 'skipped')
    failed_count = sum(1 for r in results if r['status'] == 'failed')
    
    print(f"✅ Successful: {success_count}/{len(form_ids)} forms")
    print(f"⏭️  Skipped: {skipped_count} forms (already exist)")
    print(f"❌ Failed: {failed_count} forms")
    print(f"📈 Total records fetched: {total_records:,}")
    print(f"💾 Total size: {total_size_mb:.2f} MB")
    print(f"📁 Data saved to: {OUTPUT_DIR.absolute()}")
    
    # Save summary
    summary = {
        'timestamp': datetime.now().isoformat(),
        'forms_total': len(form_ids),
        'successful': success_count,
        'skipped': skipped_count,
        'failed': failed_count,
        'total_records': total_records,
        'total_size_mb': round(total_size_mb, 2),
        'format': 'parquet',
        'results': results
    }
    
    summary_path = OUTPUT_DIR / "fetch_summary.json"
    with open(summary_path, 'w') as f:
        json.dump(summary, f, indent=2)
    
    print(f"💾 Summary saved to: {summary_path}")
    
    # List created files
    parquet_files = list(OUTPUT_DIR.glob("*.parquet"))
    if parquet_files:
        print(f"\n📁 Created {len(parquet_files)} Parquet files:")
        for f in parquet_files[:10]:  # Show first 10
            size_mb = f.stat().st_size / (1024 * 1024)
            print(f"   - {f.name} ({size_mb:.2f} MB)")
        if len(parquet_files) > 10:
            print(f"   ... and {len(parquet_files) - 10} more")
    
    print("\n🎉 Data fetch complete!")
    print("\n💡 Next step: Run R script to process Parquet files")
    print("   In R: df <- arrow::read_parquet('data/raw/4500.parquet')")

if __name__ == "__main__":
    main()