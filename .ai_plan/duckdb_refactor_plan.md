# Refactoring Plan: Single DuckDB Architecture for R Classes

## Objective
Migrate the project so all R classes in the `R` folder use a single DuckDB database file, not one per class. Table names remain clearly mapped to the classes but can be extended for multi-table classes. Each class validates and/or creates required tables.

## Refactoring Steps

### 1. Inventory and Mapping
- Create a mapping of `{R_class <-> DuckDB_table(s)}` (keep table names flexible & mutable).
- For each R class, note which tables it may manage (including current and potential future tables).

### 2. Connection Refactor
- All R classes should *accept the unified DuckDB file path as an argument* (not folder, not separate DB file per class).
- All database connections should point to the single, shared DuckDB file.

### 3. Table Validation and Creation
- On initialization, each class checks for the presence of its required tables within the DuckDB file.
- If a required table does not exist, the class creates it (with the correct schema).
- Table schemas can be managed via R code (DDL strings in the class, or a helper function).

### 4. Remove Duplicate Data & Enforce Relationships
- Remove inter-table data duplication (e.g., results referencing analysis via foreign key, not by value-copy).
- Add foreign key constraints where logical (if supported and needed).

### 5. Project Creation Change
- Project selection/creation logic should prompt for a DuckDB file, not a folder.
- If a new project is created, initialize an empty DuckDB file in the selected location.

### 6. Data Migration (One-Time Script)
- Develop a migration script:
    - Merge all existing per-class DuckDB files into the unified DB, table by table.
    - Remove duplicate or overlapping fields (resolve into proper relationships).

### 7. Documentation & User Guidance
- Document the new file/tables/class mapping and any schema evolution strategy.
- Instruct users on the new workflow (open/create a project by file, not folder).

### 8. Icon Customization (OS/UI Guidance Only)
- Document (for users) how to apply a custom icon to the .duckdb file at OS level.
   - Windows: .duckdb filetype association/registry tweak.
   - macOS: Custom icon via Get Info panel.
- If your app presents its own file browser, display a project/folder icon for DuckDB files in the UI.

## Naming Table Mapping Example  
*(Modify as appropriate in your actual implementation)*

| R Class Name                  | Default Table Name(s)      | Required Tables Mutable? |
|-------------------------------|----------------------------|--------------------------|
| Engine                        | Engine                     | Yes                      |
| MassSpecAnalyses              | MassSpecAnalyses           | Yes                      |
| MassSpecResults_NonTargetAnalysis | MassSpecResults_NonTargetAnalysis | Yes |
| Cache                         | Cache                      | Yes                      |
| ... more as discovered ...    | ...                        | Yes                      |

**Each class:**
- Checks/creates its required tables at construction.
- Accepts the main DuckDB file path as a parameter (and stores internally).

## Implementation Notes
- Use helper functions (`ensure_table_exists(con, table_name, ddl_sql)`) to DRY table validation/creation logic.
- Multi-table classes should register all their tables up front.
- Refactoring should maintain backward compatibility where possible – migrate old data if needed.

---

*Created: 2026-04-26 by opencode bot for team review and approval.*
