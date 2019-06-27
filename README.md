
purescript-ccap-codegen
=====================================

[![Build
Status](https://travis-ci.org/ccap/purescript-ccap-codegen.svg)](https://travis-ci.org/ccap/purescript-ccap-codegen)

### First time setup

```yarn```

```bower install```

### If after successful setup, pulp is not found (try pulp psci to test this)

```PATH=node_modules/.bin:$PATH```

### Testing

For the most basic testing:

    ./runtests

To additionally re-generate the auto-generated `tmpl`s from the DB, assuming you already have `~/.pgpass` set up and are on the CCAP network:

    ./runtests ccap

Print a computer-friendly (PureScript) representation of the template to the console. Useful for debugging.

    pulp run -- -m show -p unused samples/County.tmpl
