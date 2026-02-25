# Security Policy

## Supported Versions

| Version | Supported |
|---------|-----------|
| 2.1.x   | ✅        |
| 2.0.x   | ✅        |
| 1.3.x   | ✅        |
| < 1.3   | ❌        |

## Reporting a Vulnerability

Please **do not** open a public GitHub issue for security vulnerabilities.

Instead, use [GitHub private vulnerability reporting](https://github.com/saidmashhud/hookline/security/advisories/new) with:
- Description of the vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (optional)

We will respond within **48 hours** and aim to release a patch within **7 days**
for critical vulnerabilities.

## Disclosure Policy

We follow [coordinated disclosure](https://en.wikipedia.org/wiki/Coordinated_vulnerability_disclosure).
Once a fix is available, we will publish a security advisory on GitHub.

## Security Considerations

- **API keys**: Rotate `HL_API_KEY` regularly in production
- **Webhook secrets**: Use strong per-endpoint signing secrets
- **TLS**: Run behind a TLS-terminating proxy (nginx, Caddy) in production
- **Network**: Restrict `hl_store` UNIX socket permissions to the hookline user
