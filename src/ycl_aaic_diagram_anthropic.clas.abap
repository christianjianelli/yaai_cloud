CLASS ycl_aaic_diagram_anthropic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aaic_diagram_anthropic.
    INTERFACES if_oo_adt_classrun.

    ALIASES: get_chat_messages FOR yif_aaic_diagram_anthropic~get_chat_messages,
             get_diagram FOR yif_aaic_diagram_anthropic~get_diagram,
             add_participant FOR yif_aaic_diagram_anthropic~add_participant,
             add_message FOR yif_aaic_diagram_anthropic~add_message,
             parse_json FOR yif_aaic_diagram_anthropic~parse_json,
             escape_text FOR yif_aaic_diagram_anthropic~escape_text,
             mt_participants FOR yif_aaic_diagram_anthropic~mt_participants,
             mt_steps FOR yif_aaic_diagram_anthropic~mt_steps,
             mt_replacements FOR yif_aaic_diagram_anthropic~mt_replacements,
             m_diagram FOR yif_aaic_diagram_anthropic~m_diagram,
             m_maxlen FOR yif_aaic_diagram_anthropic~m_maxlen.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_diagram_anthropic IMPLEMENTATION.

  METHOD constructor.

    me->m_maxlen = 70.

    me->m_diagram = |sequenceDiagram| && cl_abap_char_utilities=>newline.

    me->mt_replacements = VALUE #(
      ( from_char = |#|  to_string = '#35;' )
      ( from_char = cl_abap_char_utilities=>newline to_string = ' ' )
    ).

  ENDMETHOD.

  METHOD get_chat_messages.

    NEW ycl_aaic_db( i_api = yif_aaic_const=>c_anthropic
                     i_id = CONV #( i_chat_id ) )->get_chat(
                                           EXPORTING
                                             i_ui = abap_false
                                           IMPORTING
                                             e_t_messages = DATA(lt_msg)
                                         ).

    LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).

      APPEND INITIAL LINE TO r_t_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

      me->parse_json(
        EXPORTING
          i_json  = <ls_msg>-msg
        IMPORTING
          e_s_msg = <ls_message>
      ).

    ENDLOOP.

  ENDMETHOD.

  METHOD parse_json.

    NEW ycl_aaic_util( )->deserialize(
      EXPORTING
        i_json = i_json
      IMPORTING
        e_data = e_s_msg
    ).

  ENDMETHOD.

  METHOD escape_text.

    " Start with the original text
    r_escaped_text = i_text.

    " Replace each special character
    LOOP AT mt_replacements ASSIGNING FIELD-SYMBOL(<ls_rep>).

      REPLACE ALL OCCURRENCES OF <ls_rep>-from_char
        IN r_escaped_text
        WITH <ls_rep>-to_string.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_participant.

    READ TABLE me->mt_participants TRANSPORTING NO FIELDS
      WITH KEY participant = to_lower( i_participant ).

    IF sy-subrc <> 0.
      APPEND VALUE #( participant = to_lower( i_participant ) ) TO me->mt_participants.
    ENDIF.

  ENDMETHOD.

  METHOD add_message.

    DATA: l_role   TYPE string,
          l_line   TYPE string,
          l_sender TYPE string,
          l_target TYPE string,
          l_len    TYPE i,
          l_3p     TYPE string.


*    DATA(ls_msg) = i_s_msg.
*
*    ls_msg-content = me->escape_text( ls_msg-content ).
*    ls_msg-arguments = me->escape_text( ls_msg-arguments ).
*    ls_msg-output = me->escape_text( ls_msg-output ).
*
*    CASE to_lower( ls_msg-type ).
*
*      WHEN 'message'.
*
*        l_sender = to_mixed( to_upper( ls_msg-role ) ).
*
*        l_target = to_mixed( to_upper( COND #( WHEN to_lower( ls_msg-role ) = 'user' THEN 'Assistant' ELSE 'User' ) ) ).
*
*      WHEN 'function_call'.
*
*        l_sender = 'Assistant'.
*
*        l_target = 'Tool'.
*
*        ls_msg-content = |Tool call: { ls_msg-name }|.
*
*      WHEN 'function_call_output'.
*
*        l_sender = 'Tool'.
*
*        l_target = 'Assistant'.
*
*        ls_msg-content = |Tool response: { ls_msg-output }|.
*
*    ENDCASE.
*
*    me->add_participant( l_sender ).
*    me->add_participant( l_target ).
*
*    l_len = COND #( WHEN strlen( ls_msg-content ) < me->m_maxlen THEN strlen( ls_msg-content ) ELSE me->m_maxlen ).
*
*    IF strlen( ls_msg-content ) > l_len.
*      l_3p = '...'.
*    ENDIF.
*
*    " Build mermaid line
*    l_line = |{ l_sender } ->> { l_target }: { substring( val = ls_msg-content off = 0 len = l_len ) }{ l_3p }|.
*
*    " Append to diagram
*    APPEND VALUE #( step = l_line && cl_abap_char_utilities=>newline ) TO mt_steps.

  ENDMETHOD.

  METHOD get_diagram.

    r_diagram = me->m_diagram.

    LOOP AT me->mt_participants ASSIGNING FIELD-SYMBOL(<ls_participant>).

      r_diagram = |{ r_diagram }participant { to_mixed( to_upper( <ls_participant>-participant ) ) }{ cl_abap_char_utilities=>newline }|.

    ENDLOOP.

    LOOP AT me->mt_steps ASSIGNING FIELD-SYMBOL(<ls_step>).

      r_diagram = |{ r_diagram }{ <ls_step>-step }|.

    ENDLOOP.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    me->get_chat_messages(
      EXPORTING
        i_chat_id    = '2A9448FCE52F1FD0ADC4327EDD69B846'
      RECEIVING
        r_t_messages = DATA(lt_messages)
    ).

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

      me->add_message( <ls_message> ).

    ENDLOOP.

    out->write( me->get_diagram( ) ).

  ENDMETHOD.

ENDCLASS.
