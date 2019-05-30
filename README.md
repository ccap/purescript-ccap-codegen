
purescript-ccap-codegen
=====================================

[![Build
Status](https://travis-ci.org/ccap/purescript-ccap-codegen.svg)](https://travis-ci.org/ccap/purescript-ccap-codegen)

### Testing

```sh
pulp run -- -m pretty -p unused county.tmpl
pulp run -- -m test -p unused county.tmpl
pulp run -- -p Ccap.Cc -m purs county.tmpl > County.purs
pulp run -- -p gov.wicourts.cc -m scala county.tmpl > County.scala
pulp run -- -m show -p unused county.tmpl
```
