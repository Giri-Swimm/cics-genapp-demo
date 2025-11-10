---
title: Endowment Policy Menu Screen
---
The Endowment Policy Menu screen allows users to inquire, add, delete, or update endowment insurance policies by entering relevant policy and customer details. It serves as the main interface for managing endowment policy records within the application.

## Screen Preview

```
SSP2        General Insurance Endowment Policy Menu

    1. Policy Inquiry 
    2. Policy Add     
    3. Policy Delete  
    4. Policy Update  

           Policy Number      ____________
           Cust Number        ____________
           Issue date         ____________ (yyyy-mm-dd)
           Expiry date        ____________ (yyyy-mm-dd)
           Fund Name          ____________
           Term               __
           Sum Assured        ______
           Life Assured       ___________________________
           With Profits       _
           Equities           _
           Managed Funds      _

    Select Option   _


[Error/Status Message Area]

ENTER=Continue  PF3=End  CLEAR=Clear
```

## Fields

### Policy Number (ENP2PNO)

- Length: 10 digits
- Input field, right-justified, zero-filled
- Used for identifying the endowment policy
- No explicit validation in the provided code, but required for inquiry, update, and delete operations

### Customer Number (ENP2CNO)

- Length: 10 digits
- Input field, right-justified, zero-filled
- Used for identifying the customer
- No explicit validation in the provided code, but required for inquiry, update, and delete operations

### Issue Date (ENP2IDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- Used for policy creation and update
- No explicit validation in the provided code

### Expiry Date (ENP2EDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- Used for policy creation and update
- No explicit validation in the provided code

### Fund Name (ENP2FNM)

- Length: 10 characters
- Input field
- Used for policy creation and update
- No explicit validation in the provided code

### Term (ENP2TER)

- Length: 2 digits
- Input field
- Used for policy creation and update
- No explicit validation in the provided code

### Sum Assured (ENP2SUM)

- Length: 6 digits
- Input field, right-justified, zero-filled
- Used for policy creation and update
- No explicit validation in the provided code

### Life Assured (ENP2LIF)

- Length: 25 characters
- Input field
- Used for policy creation and update
- No explicit validation in the provided code

### With Profits (ENP2WPR)

- Length: 1 character
- Input field
- Used for policy creation and update
- No explicit validation in the provided code

### Equities (ENP2EQU)

- Length: 1 character
- Input field
- Used for policy creation and update
- No explicit validation in the provided code

### Managed Funds (ENP2MAN)

- Length: 1 character
- Input field
- Used for policy creation and update
- No explicit validation in the provided code

### Select Option (ENP2OPT)

- Length: 1 digit
- Input field, must be entered
- Numeric only
- Used to select menu option (1-4)
- Validation: MUSTENTER (user must provide a value)

### Error/Status Message Area (ERP2FLD)

- Length: 40 characters
- Output only
- Used to display error or status messages
- Populated by the program in response to actions

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
