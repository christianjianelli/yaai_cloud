CLASS ycl_aaic_rag_db DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aaic_rag_db.

    ALIASES create FOR yif_aaic_rag_db~create.
    ALIASES read   FOR yif_aaic_rag_db~read.
    ALIASES update FOR yif_aaic_rag_db~update.
    ALIASES delete FOR yif_aaic_rag_db~delete.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_AAIC_RAG_DB IMPLEMENTATION.


  METHOD yif_aaic_rag_db~create.

    DATA: lt_aaic_rag_data TYPE STANDARD TABLE OF yaaic_rag_data,
          ls_aaic_rag      TYPE yaaic_rag.

    DATA: l_offset    TYPE i,
          l_line_no   TYPE i,
          l_remaining TYPE i.


    CLEAR e_id.

    SELECT id FROM yaaic_rag
      WHERE filename = @i_filename
      INTO @DATA(l_id)
      UP TO 1 ROWS.
    ENDSELECT.

    IF sy-subrc = 0.
      e_error = |File { i_filename } already exists in the database|.
      RETURN.
    ENDIF.

    ls_aaic_rag-id = xco_cp=>uuid( )->value.
    ls_aaic_rag-filename = i_filename.

    DATA(l_content_bin) = xco_cp=>string( i_content )->as_xstring( xco_cp_character=>code_page->utf_8 )->value.

    DATA(lo_zip) = NEW cl_abap_zip( ).

    lo_zip->add( name = i_filename
                 content = l_content_bin ).

    DATA(l_zip) = lo_zip->save( ).

    DATA(l_len) = xstrlen( l_zip ).

    DATA(l_max_len) = dbmaxlen( l_zip ).

    WHILE l_offset < l_len.

      APPEND INITIAL LINE TO lt_aaic_rag_data ASSIGNING FIELD-SYMBOL(<ls_rag_data>).

      <ls_rag_data> = CORRESPONDING #( ls_aaic_rag ).

      l_line_no = l_line_no + 1.

      l_remaining = l_len - l_offset.

      <ls_rag_data>-line_no = l_line_no.

      IF l_remaining > l_max_len.
        <ls_rag_data>-bin_data = l_zip+l_offset(l_max_len).
      ELSE.
        <ls_rag_data>-bin_data = l_zip+l_offset(l_remaining).
      ENDIF.

      l_offset = l_offset + l_max_len.

    ENDWHILE.

    IF lt_aaic_rag_data[] IS NOT INITIAL.

      INSERT yaaic_rag FROM @ls_aaic_rag.
      INSERT yaaic_rag_data FROM TABLE @lt_aaic_rag_data.

      e_id = ls_aaic_rag-id.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_rag_db~read.

    DATA l_bin_data TYPE xstring.

    CLEAR: e_filename,
           e_description,
           e_keywords,
           e_content.

    SELECT FROM yaaic_rag FIELDS id, filename, description, keywords
      WHERE id = @i_id
         OR filename = @i_filename
      INTO @DATA(ls_rag)
      UP TO 1 ROWS.                                     "#EC CI_NOORDER
    ENDSELECT.

    IF sy-subrc <> 0.
      e_error = |File { i_filename } not found in the database|.
      RETURN.
    ENDIF.

    SELECT FROM yaaic_rag_data FIELDS id, line_no, bin_data
      WHERE id = @ls_rag-id
      ORDER BY PRIMARY KEY
      INTO TABLE @DATA(lt_rag_data).

    IF sy-subrc <> 0.
      e_error = |File { i_filename } not found in the database|.
      RETURN.
    ENDIF.

    LOOP AT lt_rag_data ASSIGNING FIELD-SYMBOL(<ls_rag_data>).

      CONCATENATE l_bin_data <ls_rag_data>-bin_data INTO l_bin_data IN BYTE MODE.

    ENDLOOP.

    IF l_bin_data IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_zip) = NEW cl_abap_zip( ).

    lo_zip->load(
      EXPORTING
        zip             = l_bin_data
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_zip->get(
      EXPORTING
        name                    = CONV #( ls_rag-filename )
      IMPORTING
        content                 = DATA(l_content_bin)
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3
    ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    e_filename = ls_rag-filename.
    e_description = ls_rag-description.
    e_keywords = ls_rag-keywords.

    e_content = xco_cp=>xstring( l_content_bin )->as_string( xco_cp_character=>code_page->utf_8 )->value.

  ENDMETHOD.


  METHOD yif_aaic_rag_db~update.

    e_updated = abap_false.

    IF i_description IS NOT SUPPLIED AND i_keywords IS NOT SUPPLIED.
      RETURN.
    ENDIF.

    SELECT FROM yaaic_rag FIELDS id, description, keywords
      WHERE id = @i_id
         OR filename = @i_filename
      INTO @DATA(ls_rag)
      UP TO 1 ROWS.                                     "#EC CI_NOORDER
    ENDSELECT.

    IF sy-subrc = 0.

      IF i_description IS SUPPLIED.
        ls_rag-description = i_description.
      ENDIF.

      IF i_keywords IS SUPPLIED.
        ls_rag-keywords = i_keywords.
      ENDIF.

    ELSE.

      e_error = |File { i_filename } not found in the database|.

      RETURN.

    ENDIF.

    UPDATE yaaic_rag
      SET description = @ls_rag-description,
          keywords = @ls_rag-keywords
      WHERE id = @i_id.

    IF sy-subrc = 0.
      e_updated = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aaic_rag_db~delete.

    e_deleted = abap_false.

    DELETE FROM yaaic_rag
      WHERE id = @i_id
         OR filename = @i_filename.

    IF sy-subrc = 0.
      e_deleted = abap_true.
    ELSE.

      e_error = |File { i_filename } not found in the database|.

      RETURN.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
