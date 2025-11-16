CLASS ycl_aaic_rag_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aaic_rag_tools.

    ALIASES: get_documentation FOR yif_aaic_rag_tools~get_documentation,
             create_documentation FOR yif_aaic_rag_tools~create_documentation,
             update_documentation FOR yif_aaic_rag_tools~update_documentation,
             get_list_of_documents FOR yif_aaic_rag_tools~get_list_of_documents.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS YCL_AAIC_RAG_TOOLS IMPLEMENTATION.


  METHOD yif_aaic_rag_tools~create_documentation.

  ENDMETHOD.


  METHOD yif_aaic_rag_tools~get_documentation.

    NEW ycl_aaic_rag_db( )->read(
      EXPORTING
        i_filename = i_filename
      IMPORTING
        e_content  = r_response
    ).

  ENDMETHOD.


  METHOD yif_aaic_rag_tools~get_list_of_documents.

  ENDMETHOD.


  METHOD yif_aaic_rag_tools~update_documentation.

  ENDMETHOD.
ENDCLASS.
