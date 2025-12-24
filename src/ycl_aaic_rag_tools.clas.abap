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



CLASS ycl_aaic_rag_tools IMPLEMENTATION.


  METHOD yif_aaic_rag_tools~create_documentation.

    CLEAR r_response.

    DATA(l_filename) = i_filename.
    DATA(l_description) = i_description.
    DATA(l_file_content) = i_file_content.

    NEW ycl_aaic_rag_db( )->create(
      EXPORTING
        i_filename    = l_filename
        i_description = l_description
        i_keywords    = i_keywords
        i_content     = l_file_content
      IMPORTING
        e_id = DATA(l_id)
        e_error = DATA(l_error)
    ).

    IF l_id IS NOT INITIAL.
      r_response = |Documentation created successfully! Documentation id:"{ l_id }". Documentation filename:"{ l_filename }"|.
    ELSE.
      r_response = |Error while creating the documentation. Error: { l_error }|.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_rag_tools~get_documentation.

    CLEAR r_response.

    NEW ycl_aaic_rag_db( )->read(
      EXPORTING
        i_filename = i_filename
      IMPORTING
        e_content  = r_response
    ).

  ENDMETHOD.

  METHOD yif_aaic_rag_tools~update_documentation.

    CLEAR r_response.

    DATA(l_filename) = i_filename.
    DATA(l_description) = i_description.
    DATA(l_file_content) = i_file_content.

    NEW ycl_aaic_rag_db( )->update(
      EXPORTING
        i_filename    = l_filename
        i_description = l_description
        i_keywords    = i_keywords
        i_content     = l_file_content
      IMPORTING
        e_updated  = DATA(l_updated)
        e_error  = DATA(l_error)
    ).

    IF l_updated = abap_true.
      r_response = 'Documentation updated successfully!'.
    ELSE.
      r_response = |Error while updating the documentation. Error: { l_error }|.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_rag_tools~get_list_of_documents.

    CLEAR r_response.

    NEW ycl_aaic_rag_db( )->query(
      EXPORTING
        i_filename    = i_filename
        i_description = i_description
        i_keywords    = i_keywords
      IMPORTING
        e_t_documents = DATA(lt_documents)
    ).

    IF lt_documents IS INITIAL.
      r_response = 'No documents found.'.
      RETURN.
    ENDIF.

    r_response = /ui2/cl_json=>serialize(
      EXPORTING
        data = lt_documents
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
    ).

  ENDMETHOD.

ENDCLASS.
