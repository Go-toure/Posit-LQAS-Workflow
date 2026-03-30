#!/usr/bin/env Rscript
# Test email with Gmail App Password

library(blastula)

# Read password from .Renviron
readRenviron(".Renviron")
password <- Sys.getenv("GMAIL_APP_PASSWORD")
email_user <- Sys.getenv("EMAIL_USER", "gorguiba1@gmail.com")

if (password == "") {
  stop("GMAIL_APP_PASSWORD not set in .Renviron")
}

cat("Testing email with:", email_user, "\n")
cat("Password length:", nchar(password), "characters\n")

# Create test email
email <- compose_email(
  body = md(glue::glue("
# ✅ LQAS Email Test Successful!

**Time:** {Sys.time()}
**Email:** {email_user}

This is a test email from the LQAS Pipeline.

If you receive this, the email configuration is working correctly.

---
*WHO AFRO LQAS Polio Surveillance System*
  "))
)

# Send email using correct syntax
tryCatch({
  smtp_send(
    email,
    to = email_user,
    from = email_user,
    subject = "LQAS Pipeline - Email Test",
    credentials = creds_key(
      id = "gmail",
      user = email_user,
      pass = password
    )
  )
  cat("✅ Test email sent successfully to", email_user, "\n")
}, error = function(e) {
  cat("❌ Failed to send email:", e$message, "\n")
})
