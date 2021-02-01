# Glossary

## Balance

* **regular balance** is a balance on blockchain without any spendings;
* **outgoing leasing** is an amount of Waves which this account send to another account for leasing; 
* **pessimistic correction** is a sum of all spending by **unconfirmed transactions** of this account;
* **reserved** is all reserved by active orders assets (open volume);
* **node balance**
  ```text
  node balance = regular balance - outgoing leasing - pessimistic correction
  ```
* **tradable balance**
  ```text
  tradable balance = node balance - reserved
  ```
* **balance for audit** is a balance which is used to determine cancel orders or not.

## Transactions

* **observed transaction** is any of three: **failed**, **unconfirmed**, **confirmed**;
* **failed transaction** is a transaction which won't validated on a blockchain;
* **unconfirmed transaction** is a transaction in UTX;
* **confirmed transaction** is a transaction in a blockchain;
* **know transaction** is a transaction which is created by this Matcher. A transaction is **unknown** if it hasn't been yet created, but will be created in the future;
* **UTX** == **Mem pool**.
