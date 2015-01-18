# # Util

# Util contains helper functions.

# ## Helper functions
head = (list) -> list[0]
tail = (list) -> list[1..]
debug = true
log = (string) -> if debug then console.log string

# ## Exports
exports.head = head
exports.tail = tail
exports.debug = debug
exports.log = log
