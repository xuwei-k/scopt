```
$ sbt
set ThisBuild / resolvers += Resolver.JCenterRepository
release
exit

$ env SCALAJS_VERSION=0.6.33 sbt
++2.11.12!
scoptJS/publishSigned
++2.12.13!
scoptJS/publishSigned
++2.13.4!
scoptJS/publishSigned
```

