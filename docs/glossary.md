# Glossary

## Balance

* **regular balance**
* **outgoing leasing**
* **pessimistic correction** is a sum of all spending by **unconfirmed transactions** of this account
* **reserved** is all reserved by active orders assets (open volume)
* **node balance**
  ```text
  node balance = regular balance - outgoing leasing - pessimistic correction
  ```
* **tradable balance**
  ```text
  tradable balance = node balance - reserved
  ```

## Transactions

* **unconfirmed transactions** - transactions in UTX
* **UTX** == **Mem pool**
