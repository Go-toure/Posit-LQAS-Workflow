#!/usr/bin/env Rscript
# Test email with emayili package

library(emayili)

# Read password from .Renviron
readRenviron(".Renviron")
password <- Sys.getenv("GMAIL_APP_PASSWORD")
email_user <- Sys.getenv("EMAIL_USER", "gorguiba1@gmail.com")

if (password == "") {
  stop("GMAIL_APP_PASSWORD not set in .Renviron")
}

cat("Testing email with:", email_user, "\n")

# Create SMTP server
smtp <- server(
  host = "smtp.gmail.com",
  port = 587,
  username = email_user,
  password = password
)

# Create email
email <- envelope(
  to = email_user,
  from = email_user,
  subject = "LQAS Pipeline - Email Test",
  text = paste0(
    "LQAS Email Test Successful!\n\n",
    "Time: ", Sys.time(), "\n",
    "Email: ", email_user, "\n\n",
    "This is a test email from the LQAS Pipeline.\n\n",
    "If you receive this, the email configuration is working correctly.\n\n",
    "---\n",
    "WHO AFRO LQAS Polio Surveillance System"
  ),
  html = paste0(
    "<h1>✅ LQAS Email Test Successful!</h1>",
    "<p><strong>Time:</strong> ", Sys.time(), "</p>",
    "<p><strong>Email:</strong> ", email_user, "</p>",
    "<p>This is a test email from the LQAS Pipeline.</p>",
    "<p>If you receive this, the email configuration is working correctly.</p>",
    "<hr>",
    "<p><em>WHO AFRO LQAS Polio Surveillance System</em></p>"
  )
)

# Send email
tryCatch({
  smtp(email, verbose = TRUE)
  cat("✅ Test email sent successfully to", email_user, "\n")
}, error = function(e) {
  cat("❌ Failed to send email:", e$message, "\n")
})
