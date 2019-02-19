# Jose Acceptance Tests

[![License: MIT][MIT Badge]][MIT]
[![GitHub Release Badge]][GitHub Releases]
[![Erlang Releases Badge]][Erlang Releases]

Provides acceptance tests for [Jose](http://github.com/aialferov/jose).

## Usage

The tests run against a running Jose instance, therefore you need to provide its
host and port. Defaults are "localhost:8080".

Although running the tests from sources requires [Make] and [Erlang], you can
use [Docker] to run it without any dependency required.

Assuming you have Jose running as follows:

```
$ docker run --name jose --rm -it -p 8080:8080 aialferov/jose run
```

the tests against this instance could be run this way:

```
$ docker run --link jose --env HOST=jose --rm -it -p 8088:8088 aialferov/jose-at run
```

After tests are complete the execution continues and serves a web page with the
test run details. A link to the page is printed out and you have an option
either give it another test run or exit the execution and lose the test run
details.

### Sources

To run the tests from the source directory:

```
$ make
$ make run
```

The first step builds test suites into a binary, that could be shipped to any
machine with Erlang installed and run there. After the build the binary is
located in "_build/default/bin".

### Make Interface

[Make] interface is provided for operational purposes and is based on the [Mk]
project. Please refer the [Erlangbin.mk] and [Docker.mk] makefiles description
for details.

<!-- Links -->

[MIT]: https://opensource.org/licenses/MIT
[GitHub Releases]: https://github.com/aialferov/jose-at/releases

[Mk]: https://github.com/aialferov/mk
[Jose]: https://github.com/aialferov/jose
[Make]: https://www.gnu.org/software/make
[Docker]: https://docs.docker.io
[Erlang]: http://erlang.org
[R3tmpl]: https://github.com/aialferov/r3tmpl
[Docker.mk]: https://github.com/aialferov/mk#dockermk
[Erlangbin.mk]: https://github.com/aialferov/mk#erlangbinmk

<!-- Badges -->

[MIT Badge]: https://img.shields.io/badge/License-MIT-yellow.svg?style=flat-square
[GitHub Release Badge]: https://img.shields.io/github/release/aialferov/jose-at/all.svg?style=flat-square
[Erlang Releases]: http://www.erlang.org/news/tag/release
[Erlang Releases Badge]: https://img.shields.io/badge/Erlang-21.0%20to%2021.2-983936.svg?style=flat-square
