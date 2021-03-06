#!/bin/sh
cat lambdabot.log >> lambdabot.log.old
LC_ALL=en_US.UTF-8 PATH=$HOME/.cabal/bin:$HOME/bin:$PATH \
  exec lambdabot \
  --trust=base \
  --trust=array \
  --trust=bytestring \
  --trust=containers \
  --trust=text \
  --trust=random \
  --trust=parallel \
  --trust=stm \
  --trust=unordered-containers \
  --trust=hashable \
  --trust=exceptions \
  --trust=prelude-extras \
  --trust=base-orphans \
  --trust=bifunctors \
  --trust=profunctors \
  --trust=reflection \
  --trust=adjunctions \
  --trust=kan-extensions \
  --trust=lens \
  --trust=lambdabot-trusted \
  -X BangPatterns \
  -X ConstrainedClassMethods \
  -X ConstraintKinds \
  -X DataKinds \
  -X DeriveDataTypeable \
  -X DeriveFoldable \
  -X DeriveFunctor \
  -X DeriveGeneric \
  -X DeriveTraversable \
  -X EmptyCase \
  -X EmptyDataDecls \
  -X ExistentialQuantification \
  -X ExtendedDefaultRules \
  -X FlexibleContexts \
  -X FlexibleInstances \
  -X FunctionalDependencies \
  -X GADTs \
  -X ImplicitParams \
  -X ImplicitPrelude \
  -X KindSignatures \
  -X LiberalTypeSynonyms \
  -X MagicHash \
  -X MultiParamTypeClasses \
  -X NoMonomorphismRestriction \
  -X PackageImports \
  -X ParallelListComp \
  -X PatternGuards \
  -X PolyKinds \
  -X PolymorphicComponents \
  -X PostfixOperators \
  -X RankNTypes \
  -X ScopedTypeVariables \
  -X StandaloneDeriving \
  -X TupleSections \
  -X TypeFamilies \
  -X TypeOperators \
  -X TypeSynonymInstances \
  -X UnboxedTuples \
  -X UndecidableInstances \
  -X UnicodeSyntax \
  -X ViewPatterns \
  -e 'rc online.rc' \
  > lambdabot.log 2>&1
# --trust=semigroups
# --trust=tagged
# --trust=comonad
