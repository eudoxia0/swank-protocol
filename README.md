# swank-protocol

[![Build Status](https://travis-ci.org/eudoxia0/swank-protocol.svg?branch=master)](https://travis-ci.org/eudoxia0/swank-protocol)
[![Coverage Status](https://coveralls.io/repos/eudoxia0/swank-protocol/badge.svg?branch=master)](https://coveralls.io/r/eudoxia0/swank-protocol?branch=master)
[![Quicklisp](http://quickdocs.org/badge/swank-protocol.svg)](http://quickdocs.org/swank-protocol/)

A low-level client for the [Swank][swank] server of [SLIME][slime].

# Overview

swank-protocol is a small, low-level client for Swank. It handles connections
and reading/writing messages.

This is not a full client for Swank, it's a permissively-licensed library for
building Swank clients. It doesn't match requests to responses, it doesn't
asynchronously read responses and events from Swank. It just takes care of the
low level details: Connecting, sending messages down the socket, reading and
parsing incoming events and responses, and optionally logging.

# Usage

First, load everything:

```
(ql:quickload :swank-protocol)
```

Run this to start a Swank server on `localhost:5000`:

```lisp
(setf swank:*configure-emacs-indentation* nil)

(let ((swank::*loopback-interface* (uiop:hostname)))
  (swank:create-server :port 5000 :dont-close t))
```

Now we connect:

```
(defparameter connection
  (swank-protocol-make-connection (uiop:hostname)
                                  5000))

(swank-protocol:connect connection)
```

Now we can start sending requests:

```lisp
(swank-protocol:request-connection-info connection)
```

And reading responses:

```lisp
(swank-protocol:read-message connection)
```

For instance, let's create a REPL. First, we require some modules:

```lisp
(swank-protocol:request-swank-require connection
                                      '(swank-presentations swank-repl))
(swank-protocol:request-init-presentations connection)
```

(Don't worry about the symbols' package)

Now we actually create it:

```lisp
(swank-protocol:request-create-repl connection)
```

Now we can send things for evaluation:

```lisp
(swank-protocol:request-listener-eval connection "(+ 2 2)")
```

And receive the results:

```lisp
(swank-protocol:read-all-messages connection)
```

# API

## `connection`

The `connection` class has the following readers:

* `connection-hostname`: The Swank server's hostname.
* `connection-port`: The Swank server's port.

And the following accessors:

* `connection-request-count`: The integer ID of the last request sent to the
  Swank server. Starts at zero.
* `connection-package`: The package where things are evaluated. This should be
  changed when you send a request to Swank to change the current package.
* `connection-thread`: This is the keyword ID of the thread to execute things
  in. `t` is used by default to tell Swank to pick the default thread.
* `connection-log-p`: Whether to log messages as they are read/written.
* `connection-logging-stream`: The stream to log things to, by default,
  `*error-output*`.

Instances of `connection` can be created with `make-connection`:

```lisp
;; A regular connection
(make-connection "my-hostname" "1234")

;; A connection with logging
(make-connection "my-test-server" "1234" :logp t)
```

The `connect` function connects to the Swank server and returns t.

## Input/Output

After connecting, you can do two things: Send messages or read them.

To write messages, you can use `emacs-rex`, which takes a connection and an
S-expression to send to Swank. Implicit in this request, and stored in the
`connection` object, are two bits of information: The current package and the
request ID.

To read messages, you use `read-message`, which takes a connection and reads the
next message coming from Swank. The result is an S-expression.

## High-level Functions

There are higher-level convenience functions that call `emacs-rex`, to minimize
repetition and error:

### `connection-info`

Requests connection information. The matching response is a plist with
connection information.

Example response:

```lisp
(:return
  (:ok
    (:pid 1234
     :style :spawn
     :encoding (:coding-systems ("utf-8-unix" "iso-latin-1-unix"))
     :lisp-implementation (:type "SBCL" :name "sbcl" :version "1.2.9" :program "/usr/local/bin/sbcl")
     :machine (:instance "laptop" :type "X86-64" :version "Intel(R) Core(TM) i5-2410M CPU @ 2.30GHz")
     :features (:bordeaux-threads ... :x86-64)
     :modules ("SWANK-REPL" ... "uiop")
     :package (:name "COMMON-LISP-USER" :prompt "CL-USER")
     :version "2014-12-23"))
  1)
```

# See Also

* [swank-client][s-c]: A more complete client, but GPL-licensed.
* [Swank protocol description][description]

[slime]: https://common-lisp.net/project/slime/
[swank]: https://github.com/slime/slime/tree/master/swank
[s-c]: https://github.com/brown/swank-client
[description]: https://github.com/astine/swank-client/blob/master/swank-description.markdown

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.
