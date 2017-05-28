# letsencrypt-aws

This is a simple controller script that uses letsencrypt's [certbot][]
to create SSL certificates. It automates the `DNS-01` challenge type
using route53.

Configuration of the tool is via a json config file, with the [schema][]
defined in [ADL][].

# Usage

Create a suitable config file, and then run the tool with

```
letsencrypt-aws get-certs CONFIG.JSON
```

[certbot]:https://certbot.eff.org/
[schema]:adl/config.adl
[ADL]:https://github.com/timbod7/adl
