template-default
================

declaring Default instances just got even easier

Declaring instances of the `Default` type class has always been pretty
mechanical. This package makes the compiler do the mechanical bit.  This has
the benefit that even less thought is required, and the instance will
automatically be corrected when the definition of the data type changes (say,
to add more arguments to the constructor). Usage looks like this:

    {-# LANGUAGE TemplateHaskell #-}
    import Data.Default.TH
    deriveDefault ''MyFancyTypeName
