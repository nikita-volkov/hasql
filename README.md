# What Hasql is

Hasql is a highly efficient PostgreSQL driver and a mapping API. It targets both the users, who need a low level of abstraction, and the users, who face the typical tasks of DB-powered applications, providing them with higher-level APIs.

## Ecosystem

Hasql is not just a single library, it is a granular ecosystem of composable libraries, each isolated to perform its own task, do that well and stay simple. 

* ["hasql"](https://github.com/nikita-volkov/hasql) - the root of the ecosystem, which provides the essential abstraction over the PostgreSQL client functionality and mapping of values. Everything revolves around that library.

* ["hasql-pool"](https://github.com/nikita-volkov/hasql-pool) - a Hasql-specialized abstraction over the connection pool.

* ["hasql-transaction"](https://github.com/nikita-volkov/hasql-transaction) - a **composable** abstraction over the database transactions with an **automated conflict resolution**.

* "hasql-stream" - an abstraction over cursors. *Yet to be released.*

* ["hasql-th"](https://github.com/nikita-volkov/hasql-th) - Template Haskell utilities, such as getting the SQL from external files at compile-time. It's planned to extend this library to provide a compile-time checking of the SQL-syntax correctness.

* "hasql-migration" - an abstraction over the migration strategies. *Yet to be released.*

* ["hasql-optparse-applicative"](https://github.com/sannsyn/hasql-optparse-applicative) - "optparse-applicative" parsers for Hasql.

### Benefits of being an ecosystem

* **Simplicity.** Each library in isolation provides a simple API, which is trivial to comprehend.

* **Flexibility and composability.** The user picks and chooses the features, thus precisely matching the level of abstraction that he needs for his task.

* **Much more stable and more descriptive semantic versioning.** E.g., a change in the API of the "hasql-transaction" library won't affect any of the other libraries and it gives the user a more precise information about which part of his application he needs to update to conform.

* **Interchangeability and competition of the ecosystem components.** E.g., [not everyone will agree](https://github.com/nikita-volkov/hasql/issues/41) with the restrictive design decisions made in the "hasql-transaction" library. However those decisions are not imposed on the user, and instead of having endless debates about how to abstract over transactions, another extension library can simply be released, which will provide a different interpretation of what the abstraction over transactions should be.

* **Horizontal scalability of the ecosystem.** Instead of posting feature- or pull-requests, the users are encouraged to release their own small extension-libraries, with themselves becoming the copyright owners and taking on the maintenance responsibilities. Compare this model to the classical unscalable one, where some core-team is responsible for everything.

---

# NOTICE

With the recent releases and more to come still, the Hasql ecosystem has undergone a major revision. It is now expected that the users will need a thorough introduction. Hence this notice is to inform you that the documentation with tutorials targeted at both the newcomers and the users who need to migrate from the older versions of the ecosystem are coming soon.
