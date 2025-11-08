INTERFACE yif_aaic_rag_tools
  PUBLIC.

  METHODS get_documentation
    IMPORTING
              i_filename        TYPE yde_aaic_filename_str
    RETURNING VALUE(r_response) TYPE string.

  METHODS create_documentation
    IMPORTING
              i_filename        TYPE yde_aaic_filename_str
              i_file_content    TYPE yde_aaic_file_content_str
    RETURNING VALUE(r_response) TYPE string.

  METHODS update_documentation
    IMPORTING
              i_filename        TYPE yde_aaic_filename_str
              i_file_content    TYPE yde_aaic_file_content_str
    RETURNING VALUE(r_response) TYPE string.

  METHODS get_list_of_documents
    IMPORTING
              i_rag_id          TYPE yde_aaic_rag_id
    RETURNING VALUE(r_response) TYPE string.

ENDINTERFACE.
