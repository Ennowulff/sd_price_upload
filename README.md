# Generic SD Price Upload

[Easy and efficient way of uploading pricing conditions in SAP system using a single exclusively designed program](https://blogs.sap.com/2015/08/31/easy-and-efficient-way-of-uploading-pricing-conditions-in-sap-system-using-a-single-exclusively-designed-program/)
from blogs.sap.com by [Tarun Gambhir](https://people.sap.com/tarun.gambhir)

![image](https://github.com/Ennowulff/sd_price_upload/assets/75187288/1cbbd15a-d5f4-43bc-8bb2-af2ed82d4e37)


# Changes

I changed the import file for use with CSV format (comma separated values).

Use ";" as delimiter in your file.

# Upload structure

| **Field Names** | **Data Type** | **Description**                                             |
|-----------------|---------------|-------------------------------------------------------------|
| KAPPL           | KAPPL         | Application                                                 |
| KSCHL           | KSCHL         | Condition Type                                              |
| TABLE           | TABNAME       | Table Name                                                  |
| FLD1            | FIELDNAME     | Field Name                                                  |
| FLD2            | FIELDNAME     | Field Name                                                  |
| FLD3            | FIELDNAME     | Field Name                                                  |
| FLD4            | FIELDNAME     | Field Name                                                  |
| FLD5            | FIELDNAME     | Field Name                                                  |
| FLD6            | FIELDNAME     | Field Name                                                  |
| FLD7            | FIELDNAME     | Field Name                                                  |
| FLD8            | FIELDNAME     | Field Name                                                  |
| FLD9            | FIELDNAME     | Field Name                                                  |
| FLD10           | FIELDNAME     | Field Name                                                  |
| FLD11           | FIELDNAME     | Field Name                                                  |
| DATAB           | KODATAB       | Validity start date of the condition record                 |
| DATBI           | KODATBI       | Validity end date of the condition record                   |
| KBETR           | KBETR_KOND    | Rate (condition amount or percentage) where no scale exists |
| KPEIN           | KPEIN         | Condition pricing unit                                      |
| MEINS           | MEINS         | Base Unit of Measure                                        |
| KRECH           | KRECH         | Calculation type for condition                              |
