INTERFACE yif_aaic_rag_tools
  PUBLIC.

  METHODS get_documentation
    IMPORTING
              i_id              TYPE yde_aaic_id_str
    RETURNING VALUE(r_response) TYPE yde_aaic_response.

  METHODS create_documentation
    IMPORTING
              i_description     TYPE yde_aaic_description
              i_keywords        TYPE yde_aaic_keywords OPTIONAL
              i_content         TYPE yde_aaic_file_content_str
    RETURNING VALUE(r_response) TYPE yde_aaic_response.

  METHODS update_documentation
    IMPORTING
              i_id              TYPE yde_aaic_id_str
              i_description     TYPE yde_aaic_description OPTIONAL
              i_keywords        TYPE yde_aaic_keywords OPTIONAL
              i_content         TYPE yde_aaic_file_content_str
    RETURNING VALUE(r_response) TYPE yde_aaic_response.

  METHODS get_list_of_documents
    IMPORTING
              i_description     TYPE yde_aaic_description OPTIONAL
              i_keywords        TYPE yde_aaic_keywords OPTIONAL
    RETURNING VALUE(r_response) TYPE yde_aaic_response.

ENDINTERFACE.
