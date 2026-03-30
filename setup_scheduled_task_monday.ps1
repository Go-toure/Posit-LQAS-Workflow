# Windows PowerShell Script to Schedule LQAS Pipeline
# Run as Administrator
# Schedule: Every Monday at 11:00 AM

$taskName = "WHO_LQAS_Pipeline_Monday"
$scriptPath = "C:\Users\TOURE\Documents\Gith_repositories\Posit-LQAS-Workflow\run_production.R"
$rscriptPath = "C:\Program Files\R\R-4.4.3\bin\Rscript.exe"
$workingDir = "C:\Users\TOURE\Documents\Gith_repositories\Posit-LQAS-Workflow"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Setting up WHO LQAS Pipeline Scheduler" -ForegroundColor Cyan
Write-Host "Schedule: Every Monday at 11:00 AM" -ForegroundColor Yellow
Write-Host "========================================" -ForegroundColor Cyan

# Create action
$action = New-ScheduledTaskAction -Execute $rscriptPath -Argument $scriptPath -WorkingDirectory $workingDir

# Create trigger for Monday at 11:00 AM
$trigger = New-ScheduledTaskTrigger -Weekly -DaysOfWeek Monday -At 11:00AM

# Create settings
$settings = New-ScheduledTaskSettingsSet `
    -AllowStartIfOnBatteries `
    -DontStopIfGoingOnBatteries `
    -StartWhenAvailable `
    -RestartInterval (New-TimeSpan -Minutes 5) `
    -RestartCount 3 `
    -WakeToRun

# Register task
try {
    Register-ScheduledTask -TaskName $taskName -Action $action -Trigger $trigger -Settings $settings -Description "WHO LQAS Polio Surveillance Pipeline - Weekly Update (Every Monday 11:00 AM)" -Force
    
    Write-Host "✅ Scheduled task '$taskName' created successfully!" -ForegroundColor Green
    Write-Host "📅 Schedule: Every Monday at 11:00 AM" -ForegroundColor Yellow
    Write-Host "🔧 Working Directory: $workingDir" -ForegroundColor Gray
    Write-Host "📝 Script: $scriptPath" -ForegroundColor Gray
    Write-Host ""
    Write-Host "To view/modify task, run: taskschd.msc" -ForegroundColor Cyan
    Write-Host "To test now, run: Start-ScheduledTask -TaskName '$taskName'" -ForegroundColor Cyan
    
    # Test the task immediately
    $test = Read-Host "Do you want to test the task now? (y/n)"
    if ($test -eq 'y') {
        Write-Host "Testing task execution..." -ForegroundColor Yellow
        Start-ScheduledTask -TaskName $taskName
        Write-Host "✅ Task started! Check logs in logs/production.log" -ForegroundColor Green
    }
    
} catch {
    Write-Host "❌ Failed to create scheduled task: $_" -ForegroundColor Red
    Write-Host "Please run this script as Administrator" -ForegroundColor Yellow
}
