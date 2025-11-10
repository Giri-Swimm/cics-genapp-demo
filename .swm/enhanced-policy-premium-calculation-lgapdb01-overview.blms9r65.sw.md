---
title: Enhanced Policy Premium Calculation (LGAPDB01) - Overview
---
# Overview

## Dependencies

### Programs

- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)

### Copybooks

- SQLCA
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  kx2bb("Insurance Policy Premium Calculation Job (LGAPJOB)") --> pao3b("Enhanced Policy Premium Calculation (LGAPDB01)"):::currentEntity
click kx2bb openCode "base/cntl/lgapjob.jcl:1"
a1ktu("Handling Insurance Policy Data Inserts (LGAPOL01)") --> pao3b("Enhanced Policy Premium Calculation (LGAPDB01)"):::currentEntity
click a1ktu openCode "base/src/lgapol01.cbl:1"
  
  
click pao3b openCode "base/src/LGAPDB01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   kx2bb("Insurance Policy Premium Calculation Job (LGAPJOB)") --> pao3b("Enhanced Policy Premium Calculation (LGAPDB01)"):::currentEntity
%% click kx2bb openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%% a1ktu("Handling Insurance Policy Data Inserts (LGAPOL01)") --> pao3b("Enhanced Policy Premium Calculation (LGAPDB01)"):::currentEntity
%% click a1ktu openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%   
%%   
%% click pao3b openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
