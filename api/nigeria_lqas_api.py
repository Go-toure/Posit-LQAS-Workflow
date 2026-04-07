#!/usr/bin/env python3
"""
Nigeria LQAS Data API for EOC
Provides clean, structured LQAS data for Nigeria
"""

from fastapi import FastAPI, Query, HTTPException
from fastapi.responses import JSONResponse, FileResponse
from fastapi.middleware.cors import CORSMiddleware
import pandas as pd
import numpy as np
from datetime import datetime, date
from typing import Optional, List
import json
import os
from pathlib import Path

# Initialize FastAPI
app = FastAPI(
    title="Nigeria LQAS Data API",
    description="API for accessing cleaned LQAS data for Nigeria EOC",
    version="1.0.0",
    docs_url="/docs",
    redoc_url="/redoc"
)

# Enable CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Data path - adjust for your project
BASE_DIR = Path(__file__).parent.parent
DATA_PATH = BASE_DIR / "data/final/lqas_cleaned.csv"
CACHE_DURATION = 3600  # 1 hour cache
_cache = {"data": None, "timestamp": None}

def load_data():
    """Load and cache Nigeria data"""
    current_time = datetime.now().timestamp()
    
    if _cache["data"] is None or (current_time - (_cache["timestamp"] or 0)) > CACHE_DURATION:
        if not DATA_PATH.exists():
            return None
        df = pd.read_csv(DATA_PATH)
        # Filter for Nigeria
        df = df[df['country'] == 'NIGERIA']
        # Calculate coverage if not present
        if 'coverage' not in df.columns:
            df['coverage'] = df['total_vaccinated'] / df['total_sampled'] * 100
        _cache["data"] = df
        _cache["timestamp"] = current_time
    
    return _cache["data"]

@app.get("/")
async def root():
    return {
        "api": "Nigeria LQAS Data API",
        "version": "1.0.0",
        "endpoints": {
            "summary": "/api/summary",
            "districts": "/api/districts",
            "campaigns": "/api/campaigns",
            "trends": "/api/trends",
            "download": "/api/download",
            "states": "/api/states"
        },
        "documentation": "/docs"
    }

@app.get("/api/summary")
async def get_summary(
    start_date: Optional[str] = Query(None, description="Start date (YYYY-MM-DD)"),
    end_date: Optional[str] = Query(None, description="End date (YYYY-MM-DD)"),
    round_number: Optional[str] = Query(None, description="Round number (e.g., Rnd1, Rnd2)")
):
    """Get summary statistics for Nigeria LQAS data"""
    df = load_data()
    if df is None or df.empty:
        raise HTTPException(status_code=404, detail="No data available")
    
    # Apply filters
    if start_date:
        df = df[df['lqas_start_date'] >= start_date]
    if end_date:
        df = df[df['lqas_end_date'] <= end_date]
    if round_number:
        df = df[df['roundNumber'] == round_number]
    
    # Calculate summary
    summary = {
        "metadata": {
            "generated": datetime.now().isoformat(),
            "records": len(df),
            "filters_applied": {
                "start_date": start_date,
                "end_date": end_date,
                "round_number": round_number
            }
        },
        "overview": {
            "total_districts": int(df['district'].nunique()),
            "total_states": int(df['province'].nunique()),
            "total_campaigns": int(df['response'].nunique()),
            "total_rounds": int(df['roundNumber'].nunique()),
            "date_range": {
                "start": df['lqas_start_date'].min(),
                "end": df['lqas_start_date'].max()
            }
        },
        "coverage": {
            "total_children_sampled": int(df['total_sampled'].sum()),
            "total_children_vaccinated": int(df['total_vaccinated'].sum()),
            "overall_coverage": round(df['total_vaccinated'].sum() / df['total_sampled'].sum() * 100, 2),
            "mean_district_coverage": round(df['coverage'].mean(), 2),
            "median_district_coverage": round(df['coverage'].median(), 2),
            "min_coverage": round(df['coverage'].min(), 2),
            "max_coverage": round(df['coverage'].max(), 2)
        },
        "performance": {
            "pass_rate": round((df['status'] == 'PASS').sum() / len(df) * 100, 2),
            "pass_count": int((df['status'] == 'PASS').sum()),
            "fail_count": int((df['status'] == 'FAIL').sum()),
            "performance_breakdown": {
                "high": int((df['performance'] == 'high').sum()),
                "moderate": int((df['performance'] == 'moderate').sum()),
                "poor": int((df['performance'] == 'poor').sum()),
                "very_poor": int((df['performance'] == 'very poor').sum())
            }
        }
    }
    
    return JSONResponse(summary)

@app.get("/api/districts")
async def get_districts(
    state: Optional[str] = Query(None, description="Filter by state/province"),
    campaign: Optional[str] = Query(None, description="Filter by campaign"),
    round_number: Optional[str] = Query(None, description="Filter by round"),
    min_coverage: Optional[float] = Query(None, description="Minimum coverage percentage"),
    max_coverage: Optional[float] = Query(None, description="Maximum coverage percentage"),
    status: Optional[str] = Query(None, description="Status (PASS/FAIL)"),
    limit: int = Query(100, description="Number of records to return"),
    offset: int = Query(0, description="Number of records to skip")
):
    """Get district-level data for Nigeria"""
    df = load_data()
    if df is None or df.empty:
        raise HTTPException(status_code=404, detail="No data available")
    
    # Apply filters
    if state:
        df = df[df['province'].str.contains(state, case=False, na=False)]
    if campaign:
        df = df[df['response'].str.contains(campaign, case=False, na=False)]
    if round_number:
        df = df[df['roundNumber'] == round_number]
    if min_coverage:
        df = df[df['coverage'] >= min_coverage]
    if max_coverage:
        df = df[df['coverage'] <= max_coverage]
    if status:
        df = df[df['status'] == status.upper()]
    
    # Select columns
    result_df = df[[
        'province', 'district', 'response', 'roundNumber',
        'total_sampled', 'total_vaccinated', 'total_missed',
        'coverage', 'status', 'performance', 'lqas_start_date'
    ]].copy()
    
    # Rename columns for API
    result_df.columns = [
        'state', 'district', 'campaign', 'round',
        'children_sampled', 'children_vaccinated', 'children_missed',
        'coverage_percent', 'status', 'performance', 'assessment_date'
    ]
    
    # Apply pagination
    total = len(result_df)
    result_df = result_df.iloc[offset:offset + limit]
    
    return JSONResponse({
        "metadata": {
            "generated": datetime.now().isoformat(),
            "total_records": total,
            "returned_records": len(result_df),
            "limit": limit,
            "offset": offset
        },
        "data": result_df.to_dict(orient='records')
    })

@app.get("/api/campaigns")
async def get_campaigns():
    """Get list of all campaigns with summary statistics"""
    df = load_data()
    if df is None or df.empty:
        raise HTTPException(status_code=404, detail="No data available")
    
    campaigns = []
    for campaign in df['response'].unique():
        campaign_df = df[df['response'] == campaign]
        campaigns.append({
            "campaign_name": campaign,
            "vaccine_type": campaign_df['vaccine.type'].iloc[0] if len(campaign_df) > 0 else None,
            "rounds": campaign_df['roundNumber'].unique().tolist(),
            "districts_covered": int(campaign_df['district'].nunique()),
            "states_covered": int(campaign_df['province'].nunique()),
            "children_sampled": int(campaign_df['total_sampled'].sum()),
            "children_vaccinated": int(campaign_df['total_vaccinated'].sum()),
            "coverage": round(campaign_df['total_vaccinated'].sum() / campaign_df['total_sampled'].sum() * 100, 2),
            "pass_rate": round((campaign_df['status'] == 'PASS').sum() / len(campaign_df) * 100, 2),
            "date_range": {
                "start": campaign_df['lqas_start_date'].min(),
                "end": campaign_df['lqas_start_date'].max()
            }
        })
    
    return JSONResponse({
        "metadata": {
            "generated": datetime.now().isoformat(),
            "total_campaigns": len(campaigns)
        },
        "data": campaigns
    })

@app.get("/api/trends")
async def get_trends(
    group_by: str = Query("month", description="Group by: day, week, month, quarter, year")
):
    """Get coverage trends over time"""
    df = load_data()
    if df is None or df.empty:
        raise HTTPException(status_code=404, detail="No data available")
    
    # Convert date
    df['date'] = pd.to_datetime(df['lqas_start_date'])
    
    # Group by period
    if group_by == 'day':
        df['period'] = df['date'].dt.date
    elif group_by == 'week':
        df['period'] = df['date'].dt.to_period('W').astype(str)
    elif group_by == 'month':
        df['period'] = df['date'].dt.to_period('M').astype(str)
    elif group_by == 'quarter':
        df['period'] = df['date'].dt.to_period('Q').astype(str)
    elif group_by == 'year':
        df['period'] = df['date'].dt.year
    else:
        df['period'] = df['date'].dt.to_period('M').astype(str)
    
    # Aggregate
    trends = df.groupby('period').agg({
        'total_sampled': 'sum',
        'total_vaccinated': 'sum',
        'district': 'nunique'
    }).reset_index()
    
    trends['coverage'] = (trends['total_vaccinated'] / trends['total_sampled'] * 100).round(2)
    trends.columns = ['period', 'children_sampled', 'children_vaccinated', 'districts_assessed', 'coverage_percent']
    
    return JSONResponse({
        "metadata": {
            "generated": datetime.now().isoformat(),
            "group_by": group_by
        },
        "data": trends.to_dict(orient='records')
    })

@app.get("/api/download")
async def download_data(
    format: str = Query("csv", description="Download format: csv, json"),
    start_date: Optional[str] = Query(None),
    end_date: Optional[str] = Query(None)
):
    """Download filtered data in various formats"""
    df = load_data()
    if df is None or df.empty:
        raise HTTPException(status_code=404, detail="No data available")
    
    # Apply date filters
    if start_date:
        df = df[df['lqas_start_date'] >= start_date]
    if end_date:
        df = df[df['lqas_end_date'] <= end_date]
    
    # Select relevant columns
    export_df = df[[
        'province', 'district', 'response', 'roundNumber', 'vaccine.type',
        'lqas_start_date', 'total_sampled', 'total_vaccinated', 
        'total_missed', 'coverage', 'status', 'performance'
    ]].copy()
    
    export_df.columns = [
        'State', 'District', 'Campaign', 'Round', 'Vaccine_Type',
        'Assessment_Date', 'Children_Sampled', 'Children_Vaccinated',
        'Children_Missed', 'Coverage_Percent', 'Status', 'Performance'
    ]
    
    if format.lower() == 'csv':
        # Return as JSON response with CSV data
        csv_data = export_df.to_csv(index=False)
        return JSONResponse({
            "metadata": {
                "generated": datetime.now().isoformat(),
                "records": len(export_df),
                "format": "csv"
            },
            "data": csv_data
        })
    
    elif format.lower() == 'json':
        return JSONResponse({
            "metadata": {
                "generated": datetime.now().isoformat(),
                "records": len(export_df),
                "filters": {"start_date": start_date, "end_date": end_date}
            },
            "data": export_df.to_dict(orient='records')
        })
    
    else:
        raise HTTPException(status_code=400, detail="Format not supported. Use csv or json")

@app.get("/api/states")
async def get_states():
    """Get list of all states with summary statistics"""
    df = load_data()
    if df is None or df.empty:
        raise HTTPException(status_code=404, detail="No data available")
    
    states = []
    for state in df['province'].unique():
        state_df = df[df['province'] == state]
        states.append({
            "state": state,
            "districts": int(state_df['district'].nunique()),
            "campaigns": int(state_df['response'].nunique()),
            "children_sampled": int(state_df['total_sampled'].sum()),
            "children_vaccinated": int(state_df['total_vaccinated'].sum()),
            "coverage": round(state_df['total_vaccinated'].sum() / state_df['total_sampled'].sum() * 100, 2),
            "pass_rate": round((state_df['status'] == 'PASS').sum() / len(state_df) * 100, 2),
            "last_assessment": state_df['lqas_start_date'].max()
        })
    
    return JSONResponse({
        "metadata": {
            "generated": datetime.now().isoformat(),
            "total_states": len(states)
        },
        "data": states
    })

@app.get("/health")
async def health_check():
    """Health check endpoint"""
    df = load_data()
    return {
        "status": "healthy",
        "data_available": df is not None and not df.empty,
        "records": len(df) if df is not None else 0,
        "last_update": _cache["timestamp"],
        "api_version": "1.0.0"
    }

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
