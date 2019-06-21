
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

```sh

Pretty-print county.tmpl to the console. Removes comments and puts the template in a readable format.

```pulp run -- -m pretty -p unused county.tmpl```

Attempts to compile the county.tmpl, reporting parsing errors if not parsed correctly. NOTE: this passing does NOT guarantee a working purescript or scala file on generation; for example, if a type is undefined, no error checking catches this.

```pulp run -- -m test -p unused county.tmpl```

Generate the purescript file from the given template, and output it to County.purs

```pulp run -- -p Ccap.Cc -m purs county.tmpl > County.purs```

Generate the scala file from the given template, and output it to County.scala

pulp run -- -p gov.wicourts.cc -m scala county.tmpl > County.scala

Print a computer-friendly representation of the template to the console. Useful for debugging.

```pulp run -- -m show -p unused county.tmpl```

```
