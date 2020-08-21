# bncapi-wrapper

binance api wrapper for Common Lisp

## Usage

```lisp
> (ql:quickload :bncapi-wrapper)
> (bncapi-wrapper:trade-fee "api-key" "api-secret")
```
## Installation

1. git clone to the home directory
2. ros run
3. (asdf:initialize-source-registry '(:source-registry (:tree (:home "bncapi-wrapper")) :inherit-configuration)) 
