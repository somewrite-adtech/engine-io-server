# Engine.IO Server

This is an [Engine.IO](https://github.com/Automattic/engine.io)-compatible Server implemented in Common Lisp.

## Usage

## Installation

```common-lisp
(ql:quickload :engine-io-server)
```

## Run tests

You need Node.js and a library [engine.io-client](https://github.com/Automattic/engine.io-client) to run tests.

```common-lisp
(prove:run :t-engine-io-server)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
