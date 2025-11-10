---
title: Insurance Policy Premium Calculation Job (LGAPJOB) - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  82xtx("Insurance Policy Premium Calculation Job (LGAPJOB)"):::currentEntity --> 36x54("Enhanced Policy Premium Calculation (LGAPDB01)")
click 36x54 openCode "base/src/LGAPDB01.cbl:1"
  36x54("Enhanced Policy Premium Calculation (LGAPDB01)") --> ygpoe("LGAPDB02")
  
  
36x54("Enhanced Policy Premium Calculation (LGAPDB01)") --> su8vi("LGAPDB03")
click su8vi openCode "base/src/LGAPDB03.cbl:1"
  
  
36x54("Enhanced Policy Premium Calculation (LGAPDB01)") --> 261op("LGAPDB04")
click 261op openCode "base/src/LGAPDB04.cbl:1"
  
  
  
82xtx("Insurance Policy Premium Calculation Job (LGAPJOB)"):::currentEntity --> 90zc7("Daily Premium Summary Report Generator (LGAPRPT1)")
click 90zc7 openCode "base/src/LGAPRPT1.cbl:1"
  
  
  
click 82xtx openCode "base/cntl/lgapjob.jcl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   82xtx("Insurance Policy Premium Calculation Job (LGAPJOB)"):::currentEntity --> 36x54("Enhanced Policy Premium Calculation (LGAPDB01)")
%% click 36x54 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   36x54("Enhanced Policy Premium Calculation (LGAPDB01)") --> ygpoe("LGAPDB02")
%%   
%%   
%% 36x54("Enhanced Policy Premium Calculation (LGAPDB01)") --> su8vi("LGAPDB03")
%% click su8vi openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%   
%%   
%% 36x54("Enhanced Policy Premium Calculation (LGAPDB01)") --> 261op("LGAPDB04")
%% click 261op openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% 82xtx("Insurance Policy Premium Calculation Job (LGAPJOB)"):::currentEntity --> 90zc7("Daily Premium Summary Report Generator (LGAPRPT1)")
%% click 90zc7 openCode "<SwmPath>[base/src/LGAPRPT1.cbl](base/src/LGAPRPT1.cbl)</SwmPath>:1"
%%   
%%   
%%   
%% click 82xtx openCode "<SwmPath>[base/cntl/lgapjob.jcl](base/cntl/lgapjob.jcl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>

<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>

<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>

<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>

<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>

<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>

<SwmPath>[base/src/LGAPRPT1.cbl](base/src/LGAPRPT1.cbl)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
