# Orders' auto canceling

![How can transactions affect it](./images/oa-balance-affect.svg)

## Node's behavior

A transaction is removed from UTX only if it:
* Become failed
* Confirmed

A transaction become failed only by failing transaction's conditions (e.g. expired).

A transaction could be added to UTX again during rollbacks. This leads to events in UTX `(1)`.
```scala
// ExtensionAppender.scala:
utxStorage.addAndCleanup(droppedBlocks.flatMap { case (b, _) => b.transactionData })
```

There is also an issue when a liquid block is removed from the memory, but haven't yet written to the disk. This may lead to invalid balances responses during this time `(3)`.

## Blockchain Events (BE) Extension client behavior

In the stream a transaction may be:
* Confirmed
* Unconfirmed
* Failed

We wait for a next micro block or a block during rollbacks. This helps to minimize the number of `(1)`, but it is still possible.
We have a deduplication cache in `OrderEventsCoordinatorActor` to minimize this further `(2)`.

## Matcher Extension (ME) client behavior

During sending (see [WavesBlockchainApiGrcpService#checkedBroadcast](blob/master/waves-ext/src/main/scala/com/wavesplatform/dex/grpc/integration/services/WavesBlockchainApiGrpcService.scala#L153)) we can observe that a transaction:
* Is confirmed
* Is not confirmed and was added
* Is not confirmed and was in UTX before
* Become failed, and we can't retry because of transaction's issues
* Become failed, and we can retry (see [WavesBlockchainApiGrcpService#canRetry](blob/master/waves-ext/src/main/scala/com/wavesplatform/dex/grpc/integration/services/WavesBlockchainApiGrpcService.scala#L183)):
  * Transaction doesn't fit UTX either by number or size limit;
  * Concurrency issues (see "Too much" & "negative asset balance" in the code).

## Cases

AddressActor should receive events about a transaction only once. `(2)` protects against such situations.

⛔️ no retries
꩜️ retries are allowed
🆕 a transaction is new
💿 a transaction already in the UTX

🟥 - highly possible
🟧 - possible
🟨 - happens rarely
⬜ - impossible

Here is not all cases, but most interesting.

### When another Matcher works faster

| # |    BE   |       ME     |     BE    |Is Possible|Description|
|---|---------|--------------|-----------|:---------:|-----------|
| 1 |confirmed|confirmed     |confirmed  |    🟧     |During rollbacks. UtxStream should send unconfirmed first `(1)` 
| 2 |confirmed|confirmed     |unconfirmed|    🟧     |During rollbacks `(1)`
| 3 |confirmed|confirmed     |failed     |    ⬜     |Almost impossible, during rollbacks
| 4 |confirmed|unconfirmed 🆕|confirmed  |    ⬜     |Almost impossible, but totally fine
| 5 |confirmed|unconfirmed 🆕|unconfirmed|    🟨     |During rollbacks `(1)` and between removing a block and adding transactions to UTX
| 6 |confirmed|unconfirmed 🆕|failed     |    ⬜     |A transaction either valid or invalid
| 7 |confirmed|unconfirmed 💿|confirmed  |    🟥     |Normal condition
| 8 |confirmed|unconfirmed 💿|unconfirmed|    🟨     |During rollbacks `(1)` and between removing a block and adding transactions to UTX
| 9 |confirmed|unconfirmed 💿|failed     |    ⬜     |A transaction becomes invalid during rollbacks. Possible for scripted assets, for normal assets this should be almost impossible
|10 |confirmed|failed ⛔️     |confirmed  |    ⬜     |Impossible on Node. Transaction either valid or not.
|11 |confirmed|failed ⛔️     |unconfirmed|    🟨     |If transaction becomes invalid in UTX and removed from it. But we haven't received a message in UTX
|12 |confirmed|failed ⛔️     |failed     |    🟨     |During rollbacks `(1)`
|13 |confirmed|failed ꩜️     |confirmed  |    🟨     |During rollbacks `(1)` and full UTX
|14 |confirmed|failed ꩜️     |unconfirmed|    🟨     |During rollbacks `(1)` and full UTX
|15 |confirmed|failed ꩜️     |failed     |    🟨     |During rollbacks `(1)`, when a transaction becomes invalid, e.g. by timestamp
|16 |unconfirmed|confirmed     |confirmed  |    🟥     |Normal condition
|17 |unconfirmed|confirmed     |unconfirmed|    🟧     |During rollbacks `(1)`
|18 |unconfirmed|confirmed     |failed     |    ⬜     |A transaction either valid or invalid
|19 |unconfirmed|unconfirmed 🆕|confirmed  |    🟨     |Almost impossible, during `(3)` 
|20 |unconfirmed|unconfirmed 🆕|unconfirmed|    🟨     |Almost impossible, during `(3)`
|21 |unconfirmed|unconfirmed 🆕|failed     |    🟥     |A transaction becomes invalid. Possible for scripted assets, for normal assets this should be rare
|22 |unconfirmed|unconfirmed 💿|confirmed  |    🟥     |Normal condition
|23 |unconfirmed|unconfirmed 💿|unconfirmed|    🟥     |Normal condition
|24 |unconfirmed|unconfirmed 💿|failed     |    🟥     |A transaction becomes invalid. Possible for scripted assets, for normal assets this should be rare
|25 |unconfirmed|failed ⛔️     |confirmed  |    ⬜     |A transaction either valid or invalid
|26 |unconfirmed|failed ⛔️     |unconfirmed|    ⬜     |A transaction either valid or invalid
|27 |unconfirmed|failed ⛔️     |failed     |    🟥     |A transaction becomes invalid. Possible for scripted assets, for normal assets this should be rare
|28 |unconfirmed|failed ꩜️     |confirmed  |    🟨     |Concurrency issues
|29 |unconfirmed|failed ꩜️     |unconfirmed|    🟨     |Concurrency issues
|30 |unconfirmed|failed ꩜️     |failed     |    🟨     |Concurrency issues or a transaction becomes invalid
|31 |failed     |confirmed     |confirmed  |    ⬜     |During rollbacks, almost impossible
|32 |failed     |confirmed     |unconfirmed|    ⬜     |A transaction either valid or invalid
|33 |failed     |confirmed     |failed     |    ⬜     |A transaction either valid or invalid
|34 |failed     |unconfirmed 🆕|confirmed  |    ⬜     |A transaction either valid or invalid
|35 |failed     |unconfirmed 🆕|unconfirmed|    ⬜     |A transaction either valid or invalid
|36 |failed     |unconfirmed 🆕|failed     |    ⬜     |A transaction either valid or invalid
|37 |failed     |unconfirmed 💿|confirmed  |    ⬜     |A transaction either valid or invalid
|38 |failed     |unconfirmed 💿|unconfirmed|    ⬜     |A transaction either valid or invalid
|39 |failed     |unconfirmed 💿|failed     |    ⬜     |A transaction either valid or invalid
|40 |failed     |failed ⛔️     |confirmed  |    ⬜     |During rollbacks, almost impossible
|41 |failed     |failed ⛔️     |unconfirmed|    ⬜     |A transaction either valid or invalid
|42 |failed     |failed ⛔️     |failed     |    🟥     |An invalid transaction. Possible for scripted assets, for normal assets this should be rare
|43 |failed     |failed ꩜️     |confirmed  |    ⬜     |During rollbacks, almost impossible
|44 |failed     |failed ꩜️     |unconfirmed|    ⬜     |A transaction either valid or invalid
|45 |failed     |failed ꩜️     |failed     |    ⬜     |A transaction either valid or invalid
