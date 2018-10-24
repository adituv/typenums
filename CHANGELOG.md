Change log
==========

typenums uses [PVP Versioning][1].
The change log is available [on GitHub][2].

0.1.2.1
=======
* Fix build failure on GHC 8.6 by adding conditional NoStarIsType langauge
pragma

0.1.2
=====
* ([#2](https://github.com/adituv/typenums/issues/2))
  Refactored type-level arithmetic so that the type families are exposed from
  an Internal module.

0.1.1.1
=======
* Add UndecidableInstances language extension to Data.TypeNums.Rats.  This
  fixes a compilation error with GHC HEAD.

0.1.1
=====
* ([#1](https://github.com/adituv/typenums/issues/1))
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

