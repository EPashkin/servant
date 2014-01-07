Servant server
============

Servant application (server) in Erlang.

Servant application semiautomates this workflow:<br>
User has many archives (rar, zip) in single folder (its manga for me)<br>
Periodically some archives:
  * unzipped to subfolder with same name
  * complemented
  * archived back in subfolder
  * replaced old archive with new

Features
--------

Servant server can detect and after user confirmation do this task:
* subfolder need archivation: contains files and folders, but no archives; currently using winrar for archivation
* subfolder only contains archive (with same name), thus archive can be moved to parent folder
* subfolder is empty and can be deleted

Second component [ServantTray] (https://github.com/EPashkin/ServantTray) is responsible for the interaction with the user.
He receives list of confirmation from server and showing it in content menu of tray icon.
After user click menu item, ServantTray sends confirmation to server.


Install
-------

Servant requires [Erlang/OTP][1], [git] [2] and [rebar][3].<br>
To get source via git from this repository.
```sh
git clone https://github.com/EPashkin/servant.git
```

To get all dependencies, go to the servant directory and type:
```sh
rebar get-deps
```
or on Windows
```bat
escript rebar get-deps
```

And to build meck and other dependencies:
```sh
rebar compile
```
or on Windows
```bat
escript rebar compile
```

Build and start server
-----

a) To build Servant and run tests, go to the servant directory and type:
```sh
rebar compile eunit skip_deps=true
```
Two things might seem alarming when running the tests:
  1. Warnings emitted by cover
  2. En exception printed by SASL
Both are expected due to the way Erlang currently prints errors. The
important line you should look for is `All XX tests passed`, if that
appears all is correct.

b) To generate release:<br>
```sh
rebar generate
```

c) To start server:<br>
on Windows (service currently not working)
```bat
rel\servant_node\bin\servant_node.cmd console
```

Contribute
----------

Patches are greatly appreciated! For a much nicer history, please
write good commit messages. Use a branch name prefixed by
`feature/` (e.g. `feature/my_example_branch`) for easier integration
when developing new features or fixes for servant.

Should you find yourself using Servant and have issues, comments or
feedback please [create an issue here on GitHub][4].

Contributors:
<br>none now

  [1]: http://www.erlang.org "Erlang/OTP"
  [2]: http://git-scm.com/ "Git - distributed version control system"
  [3]: https://github.com/basho/rebar "Rebar - A build tool for Erlang"
  [4]: https://github.com/EPashkin/servant/issues "Servant issues"
