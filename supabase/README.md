# OFred accounts
1. Supabase SQL Editor: run supabase/setup-accounts.sql once.
2. Authentication > URL Configuration: Site URL https://ofredstats.com and allowed Redirect URL https://ofredstats.com/account.html.
3. Authentication > Providers: enable email/password and email confirmation. Configure a production SMTP provider before inviting many users; built-in delivery is restricted.
4. Create your account at /account.html, confirm the email, then approve it in SQL Editor using the email approval query at the bottom of setup-accounts.sql.
5. Friends sign up and confirm email. Approve each explicitly with that same query. No account can approve itself. No client insert/update/delete grants exist.
6. Smoke test with a signed-out browser, an unapproved account, then an approved account. Only approved accounts should see the board. Confirm direct anonymous API reads are rejected and authenticated unapproved reads return no board rows.
7. New picks must be published to public.laymaker_boards, id=current, payload JSON. Never embed private picks in HTML or commit them to public GitHub. Update via trusted Supabase SQL Editor/Table Editor; do not add service-role keys to the browser.
8. To revoke a friend, set approved=false. This prevents future reads; already viewed information cannot be recalled.
9. The signup screen uses the public publishable key. Never put database passwords or service-role keys into the repository.
Payments are not implemented. Approval is the only entitlement for now. Historical public commits can still contain older picks; this protects future database-only content.
