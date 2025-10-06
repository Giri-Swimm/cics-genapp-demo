---
title: General Insurance Endowment Policy Menu
---
The Endowment Policy Menu screen allows users to inquire, add, delete, or update endowment insurance policies by entering relevant policy and customer details. It serves as the main interface for managing endowment policies within the application.

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

        Select Option         _


        [Status/Error Message Area]

ENTER=Continue  PF3=End  CLEAR=Clear
```

## Fields

### Policy Number (ENP2PNO)

- Length: 10 characters
- Input field, right-justified, zero-filled
- Used for identifying the endowment policy
- No explicit validation in the provided code, but must be numeric as per COBOL definition

### Customer Number (ENP2CNO)

- Length: 10 characters
- Input field, right-justified, zero-filled
- Used for identifying the customer
- No explicit validation in the provided code, but must be numeric as per COBOL definition

### Issue Date (ENP2IDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- No explicit validation in the provided code, but expected to be a valid date

### Expiry Date (ENP2EDA)

- Length: 10 characters
- Input field
- Format: yyyy-mm-dd
- No explicit validation in the provided code, but expected to be a valid date

### Fund Name (ENP2FNM)

- Length: 10 characters
- Input field
- No explicit validation in the provided code

### Term (ENP2TER)

- Length: 2 characters
- Input field
- Numeric (as per COBOL definition)
- No explicit validation in the provided code

### Sum Assured (ENP2SUM)

- Length: 6 characters
- Input field, right-justified, zero-filled
- Numeric (as per COBOL definition)
- No explicit validation in the provided code

### Life Assured (ENP2LIF)

- Length: 25 characters
- Input field
- No explicit validation in the provided code

### With Profits (ENP2WPR)

- Length: 1 character
- Input field
- No explicit validation in the provided code
- Typically expects Y/N or similar, but not enforced in code

### Equities (ENP2EQU)

- Length: 1 character
- Input field
- No explicit validation in the provided code
- Typically expects Y/N or similar, but not enforced in code

### Managed Funds (ENP2MAN)

- Length: 1 character
- Input field
- No explicit validation in the provided code
- Typically expects Y/N or similar, but not enforced in code

### Select Option (ENP2OPT)

- Length: 1 character
- Input field, numeric only
- Must be entered (MUSTENTER validation)
- Used to select menu option (1-4)
- If invalid, error message is displayed

### Status/Error Message Area (ERP2FLD)

- Length: 40 characters
- Output only, protected
- Displays status or error messages (e.g., 'Life Policy Deleted', 'Error Updating Life Policy')
- Set by program logic based on transaction outcome

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
