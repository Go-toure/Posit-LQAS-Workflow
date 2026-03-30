@echo off
echo ========================================
echo Scheduling Monday LQAS Pipeline
echo ========================================

SCHTASKS /CREATE /TN "WHO_LQAS_Monday_Pipeline" ^
    /TR "C:\Program Files\Git\bin\bash.exe -c 'cd /c/Users/TOURE/Documents/Gith_repositories/Posit-LQAS-Workflow && Rscript run_pipeline.R && git add . && git commit -m \"Monday update: $(date +%%Y-%%m-%%d)\" && git push'" ^
    /SC WEEKLY ^
    /D MON ^
    /ST 11:00 ^
    /RL HIGHEST ^
    /F

if %errorlevel% equ 0 (
    echo ✅ Task created successfully!
    echo 📅 Schedule: Every Monday at 11:00 AM
    echo 📁 Repository: Posit-LQAS-Workflow
    echo.
) else (
    echo ❌ Failed to create task. Run as Administrator.
)

pause
