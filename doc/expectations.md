
## ToDo

- Define indexes on materialized views in `entity`. Possibly sort by orderpos.
- May define views `child_nodes` and `parent_nodes`. Useful for nicer join code?
- Respect access rights.


## Assumptions

- `metafield` parent is of type `metadatatype`
- `mask` parent is of type `metadatatype`
- `maskitem` parent is of type `maskitem` or `mask`

- has exactly one parent: `mask`, `maskitem`, `mappingfield`
- `mappingfield` has no children nodes

- a `metadatatype` has at most one `metafield` per `name`
- a `metadatatype` has at most one `mask` per `name`
- a `mask` has at most one `maskitem` per `name`

- a `maskitem` is one of these cases:
    - has at least one child, and ...
        - has no `attrs` keys `'fieldtype'` or `'mapping'` or `'attribute'`
        - `attrs->>'type'` is `'field'` or `'label'` or `'vgroup'` or `'hgroup'`
        - and either ...
            - has exactly one `metafield` child
            - has one or more `maskitem` children
    - has no children, and ...
        - `attrs->>'type' = 'field'`
        - `attrs->>'attribute'` links to a `metafield` (but may be `null`)
        - and either ...
            - `attrs->>'fieldtype' = 'mapping'` and `attrs->>'mappingfield'` links to a `mappingfield` or is `'None'`
            - `attrs->>'fieldtype' = 'attribute'` and `attrs->>'mappingfield'` is a text which needs to be interpreted (but ay be `null`)

 - `metadatatype` nodes are the same whether...
     - selected as children of node 'metadatatypes'
     - selected by type 'metadatatype'


## Questions

- What indexes are created dynamically / are not in the dump

- Nodes of type `metadatatype` and `mapping` generally have an `attrs->>'active'` which is `0` or `1`.  
    - Should we only consider those nodes which are marked as active?
    - There are also some `mapping` nodes, that don't have this `attrs` key.
      Counts in examined production database are: 37x 1, 1x 0, 8x absent.

### Presumable inconsistencies in production database

- There are `metafield` nodes, that aren't assigned to a `metadatatype` (e.g. 1342371)

- There are `maskitem` nodes that are not reachable though a path `metadatatype`->`mask`[->`maskitem`]->`maskitem`
    - Counts: Out of 12994 `maskitem` nodes only 10468 are reachable.

- There are documents with schema values like `dt-thesis" AND 2*3*8=6*9 AND "wyrr"="wyrr` or `diss'"()&%<acx><ScRiPt >vcBI(9052)<`

## Suggestions

- Index `mediatum.node (type)` (Currently only partial index. Correct?)
- Index `mediatum.node (id)` (already existing?)
- Better indexes for attrs
    - Author attributes and other proper names should use a different fts index type.
        - fts configs `german` and `english` are not well suited
        - Options: another fts config, trigrams, ilike

## Ideas

- Highlightning matching text in search results
- Search-as-you-type
    - Throttling, waiting, max request rate
    - Extra API functions
    - Delayed sorting
    - ...
- Entity for a (poss. ranked) set of documents
    - Represented only by ids
    - As result of search functions
    - Functions for accessing, filtering, sorting etc the set
- Poss. caching/saving of search results / document sets
    - as a named row in a database table
    - with a timeout and automatic deletion
