# Cl-booru

A common lisp implementation of a Danbooru like image board. This was built for a very unsuccessful site that has since closed.

## Caveats/bugs

At this moment I don't have a distributable copy of the postgress Schema that goes with this. Most of the schema can be derived from the Postmodern DAO classes in the types directory though. I will hopefully release a suitible schema dump but didn't want to delay releasing this any longer.

Image searching has some bugs right now. The most visible is there might be duplicate results returned for complex searches that use wildcards. I believe this is a regression that was introduced when I was trying to optimize searches.

This was written before I discovered unit testing. There may be undiscovered bugs. You have been warned.
