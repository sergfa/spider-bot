# spider-bot
A library and tool to disovery web site pages.

#  Installing Haskell

## Use Stack to get going with Haskell

Get [Stack](http://haskellstack.org) to get GHC installed and to build your projects.

#  Running tests

stack test

#  Build

stack build

# Running

stack exec discovery 10 https://www.iana.org/domains/reserved iana.json

```js
{
    "pages": [
        {
            "url": "https://www.iana.org/",
            "title": "Internet Assigned Numbers Authority\n"
        },
        {
            "url": "https://www.iana.org/about",
            "title": "IANA — About us\n"
        },
        {
            "url": "https://www.iana.org/domains",
            "title": "IANA — Domain Name Services\n"
        },
        {
            "url": "https://www.iana.org/domains/reserved",
            "title": "IANA — IANA-managed Reserved Domains\n"
        },
        {
            "url": "https://www.iana.org/domains/root/db/xn--0zwm56d.html",
            "title": "IANA — .测试 Domain Delegation Data\n"
        },
        {
            "url": "https://www.iana.org/domains/root/db/xn--80akhbyknj4f.html",
            "title": "IANA — .испытание Domain Delegation Data\n"
        },
        {
            "url": "https://www.iana.org/domains/root/db/xn--g6w251d.html",
            "title": "IANA — .測試 Domain Delegation Data\n"
        },
        {
            "url": "https://www.iana.org/domains/root/db/xn--hgbk6aj7f53bba.html",
            "title": "IANA — .آزمایشی Domain Delegation Data\n"
        },
        {
            "url": "https://www.iana.org/domains/root/db/xn--kgbechtv.html",
            "title": "IANA — .إختبار Domain Delegation Data\n"
        },
        {
            "url": "https://www.iana.org/numbers",
            "title": "IANA — Number Resources\n"
        },
        {
            "url": "https://www.iana.org/protocols",
            "title": "IANA — Protocol Registries\n"
        }
    ],
    "landingPage": "https://www.iana.org/domains/reserved",
    "timestamp": 1539960783
}
```
