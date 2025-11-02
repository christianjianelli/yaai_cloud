CLASS ycl_aaic_rag_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS get_documentation
      IMPORTING
                i_filename        TYPE string
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_rag_tools IMPLEMENTATION.

  METHOD get_documentation.

    NEW ycl_aaic_rag_db( )->read(
      EXPORTING
        i_filename = i_filename
      IMPORTING
        e_content  = r_response
    ).

  ENDMETHOD.

ENDCLASS.
