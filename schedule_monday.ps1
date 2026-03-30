# Schedule Monday 11:00 AM Pipeline Run
$taskName = "LQAS_Monday_Pipeline"
$scriptPath = "C:\Users\TOURE\Documents\Gith_repositories\Posit-LQAS-Workflow\monday_morning.sh"
$bashPath = "C:\Program Files\Git\bin\bash.exe"

# Create action
$action = New-ScheduledTaskAction -Execute $bashPath -Argument $scriptPath

# Create trigger for Monday at 11:00 AM
$trigger = New-ScheduledTaskTrigger -Weekly -DaysOfWeek Monday -At 11:00AM

# Create settings
$settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable

# Register task
Register-ScheduledTask -TaskName $taskName -Action $action -Trigger $trigger -Settings $settings -Force

Write-Host "✅ Scheduled task '$taskName' created" -ForegroundColor Green
Write-Host "📅 Runs every Monday at 11:00 AM" -ForegroundColor Yellow
Write-Host "📁 Script: $scriptPath" -ForegroundColor Cyan
