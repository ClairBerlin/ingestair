# Ingestair

This is an alternate implementation of the sample data ingestion engine _ingestair_.
In the regular clair-stack, the ingestair is a Django ReST application whose sole purpose is to receive samples as POST request and to persist them in the database shared with the _managair_ application.

This alternate implementation is written in the purely functional programming language _Haskell_, just for the fun of it.
My goal with this implementation is to understand how to best structure a real-world production-ready web application in Haskell, which libraries to use, how to do error handling, logging, and similar production issues.
Even though the functionality is minimal, I tried to come up with a "grown up" application architecture that follows Robert C. Martin's "Clean Architecture" principles.
Thus, some things might seem overengineered because they are for an application of this size.
