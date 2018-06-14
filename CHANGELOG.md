Change log
==========

typenums uses [PVP Versioning][1].
The change log is available [on GitHub][2].

0.1.1
=====
* [#1](https://github.com/adituv/typenums/issues/1)
  Added existentially-quantified datatypes SomeInt and SomeRat to handle
  type-level Ints and Rats that are not statically known.  Added functions
  someIntVal and someRatVal to construct these from an Integer/Rational value.

0.1.0.0
=======
* Initial Haddock release
* Defined type-level integers and rationals
* Defined polykinded arithmetic over nats, ints and rats
* Defined polykinded comparison over nats, ints and rats 
* Added Data.TypeLits module

[1]: https://pvp.haskell.org
[2]: https://github.com/adituv/typenums/releases

