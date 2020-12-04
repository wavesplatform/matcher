# How Matcher interacts with the Waves Node

It requires two installed extensions on the Waves Node:
* `Blockchain Updates` provides a stream of blockchain events: a block appended, a chain rolled back, etc.;
* `Matcher Extension` provides other required functionality: getting balances, a stream of UTX events, etc.

![do](./images/wni-ext.svg)

The clients of these extensions are coupled. This means when one of them stops, another stops too.

Blockchain Updates
The client could be described as a state machine:
