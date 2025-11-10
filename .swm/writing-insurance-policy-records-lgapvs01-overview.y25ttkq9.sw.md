---
title: Writing Insurance Policy Records (LGAPVS01) - Overview
---
# Overview

This document explains the flow for packaging and routing incoming insurance policy data. The process determines the policy type, maps product-specific details, and stores the record. If storage fails, error details are captured and routed to logging queues.

## Dependencies

### Programs

- LGAPVS01 (<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  dnn72("Inserting Insurance Policy Data (LGAPDB09)") --> 4q9qv("Writing Insurance Policy Records (LGAPVS01)"):::currentEntity
click dnn72 openCode "base/src/lgapdb09.cbl:1"
  
  
click 4q9qv openCode "base/src/lgapvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   dnn72("Inserting Insurance Policy Data (LGAPDB09)") --> 4q9qv("Writing Insurance Policy Records (LGAPVS01)"):::currentEntity
%% click dnn72 openCode "<SwmPath>[base/src/lgapdb09.cbl](base/src/lgapdb09.cbl)</SwmPath>:1"
%%   
%%   
%% click 4q9qv openCode "<SwmPath>[base/src/lgapvs01.cbl](base/src/lgapvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
