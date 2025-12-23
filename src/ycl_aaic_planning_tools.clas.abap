CLASS ycl_aaic_planning_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS create_plan
      IMPORTING
                i_plan            TYPE string
      RETURNING VALUE(r_response) TYPE yde_aaic_response.

    METHODS get_plan
      IMPORTING
                i_id              TYPE string
      RETURNING VALUE(r_response) TYPE yde_aaic_response.

    METHODS update_plan
      IMPORTING
                i_id              TYPE string
                i_plan            TYPE string
      RETURNING VALUE(r_response) TYPE yde_aaic_response.

    METHODS delete_plan
      IMPORTING
                i_id              TYPE string
      RETURNING VALUE(r_response) TYPE yde_aaic_response.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_planning_tools IMPLEMENTATION.

  METHOD create_plan.

    CLEAR r_response.

    IF i_plan IS INITIAL.
      r_response = 'The plan content is mandatory.'.
      RETURN.
    ENDIF.

    NEW ycl_aaic_rag_db( )->create(
      EXPORTING
        i_filename    = |{ xco_cp=>uuid( )->value }.md|
        i_content     = i_plan
      IMPORTING
        e_id          = DATA(l_id)
        e_error       = DATA(l_error)
    ).

    IF l_error IS NOT INITIAL.
      r_response = |Error: { l_error }|.
      RETURN.
    ENDIF.

    IF l_id IS NOT INITIAL.
      r_response = 'An unexpected error occurred'.
      RETURN.
    ENDIF.

    r_response = |Plan created successfully! The plan id is: { l_id }|.

  ENDMETHOD.

  METHOD get_plan.

    CLEAR r_response.

    IF i_id IS INITIAL.
      r_response = 'The plan id is mandatory.'.
      RETURN.
    ENDIF.

    NEW ycl_aaic_rag_db( )->read(
      EXPORTING
        i_id          = CONV #( i_id )
      IMPORTING
        e_content     = r_response
        e_error       = DATA(l_error)
    ).

    IF l_error IS NOT INITIAL.
      r_response = |Error: { l_error }|.
      RETURN.
    ENDIF.

    IF r_response IS NOT INITIAL.
      r_response = |Plan { i_id } not found.|.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD update_plan.

    CLEAR r_response.

    IF i_id IS INITIAL.
      r_response = 'The plan id is mandatory.'.
      RETURN.
    ENDIF.

    IF i_plan IS INITIAL.
      r_response = 'The plan content is mandatory.'.
      RETURN.
    ENDIF.

    NEW ycl_aaic_rag_db( )->update(
      EXPORTING
        i_id          = CONV #( i_id )
        i_content     = i_plan
      IMPORTING
        e_updated     = DATA(l_updated)
        e_error       = DATA(l_error)
    ).

    IF l_error IS NOT INITIAL.
      r_response = |Error: { l_error }|.
      RETURN.
    ENDIF.

    IF l_updated = abap_true.
      r_response = r_response = |Plan { i_id } updated successfully!|.
    ELSE.
      r_response = 'An unexpected error occurred'.
    ENDIF.

  ENDMETHOD.

  METHOD delete_plan.

    CLEAR r_response.

    IF i_id IS INITIAL.
      r_response = 'The plan id is mandatory.'.
      RETURN.
    ENDIF.

    NEW ycl_aaic_rag_db( )->delete(
      EXPORTING
        i_id       = CONV #( i_id )
      IMPORTING
        e_deleted  = DATA(l_deleted)
        e_error    = DATA(l_error)
    ).

    IF l_error IS NOT INITIAL.
      r_response = |Error: { l_error }|.
      RETURN.
    ENDIF.

    IF l_deleted = abap_true.
      r_response = r_response = |Plan { i_id } deleted successfully!|.
    ELSE.
      r_response = 'An unexpected error occurred'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
