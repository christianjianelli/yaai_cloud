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
             add_step FOR yif_aaic_diagram_anthropic~add_step,
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

  METHOD yif_aaic_diagram_anthropic~get_chat_messages.

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

  METHOD yif_aaic_diagram_anthropic~parse_json.

    NEW ycl_aaic_util( )->deserialize(
      EXPORTING
        i_json = i_json
      IMPORTING
        e_data = e_s_msg
    ).

  ENDMETHOD.

  METHOD yif_aaic_diagram_anthropic~escape_text.

    " Start with the original text
    r_escaped_text = i_text.

    " Replace each special character
    LOOP AT mt_replacements ASSIGNING FIELD-SYMBOL(<ls_rep>).

      REPLACE ALL OCCURRENCES OF <ls_rep>-from_char
        IN r_escaped_text
        WITH <ls_rep>-to_string.

    ENDLOOP.

  ENDMETHOD.

  METHOD yif_aaic_diagram_anthropic~add_participant.

    READ TABLE me->mt_participants TRANSPORTING NO FIELDS
      WITH KEY participant = to_lower( i_participant ).

    IF sy-subrc <> 0.
      APPEND VALUE #( participant = to_lower( i_participant ) ) TO me->mt_participants.
    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_diagram_anthropic~add_step.

    DATA: l_line    TYPE string,
          l_content TYPE string,
          l_len     TYPE i,
          l_suffix  TYPE string.

    me->add_participant( i_sender ).
    me->add_participant( i_target ).

    l_content = me->escape_text( i_content ).

    l_len = COND #( WHEN strlen( l_content ) < me->m_maxlen THEN strlen( l_content ) ELSE me->m_maxlen ).

    IF strlen( i_content ) > l_len.
      l_suffix = '...'.
    ENDIF.

    " Build mermaid line
    l_line = |{ i_sender } ->> { i_target }: { substring( val = l_content off = 0 len = l_len ) }{ l_suffix }|.

    " Append to diagram
    APPEND VALUE #( step = l_line && cl_abap_char_utilities=>newline ) TO mt_steps.

  ENDMETHOD.

  METHOD yif_aaic_diagram_anthropic~add_message.

    DATA lt_anthropic_chat_response TYPE STANDARD TABLE OF yif_aaic_anthropic=>ty_response_content_s.

    DATA: l_role   TYPE string,
          l_line   TYPE string,
          l_sender TYPE string,
          l_target TYPE string,
          l_len    TYPE i,
          l_suffix TYPE string.


    DATA(ls_msg) = i_s_msg.

    IF ls_msg-role = 'user'.

      FREE lt_anthropic_chat_response.

      /ui2/cl_json=>deserialize(
        EXPORTING
          json = ls_msg-content
        CHANGING
          data = lt_anthropic_chat_response
      ).

      IF lt_anthropic_chat_response IS NOT INITIAL.

        CLEAR ls_msg-content.

        LOOP AT lt_anthropic_chat_response ASSIGNING FIELD-SYMBOL(<ls_anthropic_chat_response>).

          CASE <ls_anthropic_chat_response>-type.

            WHEN 'text'.

              l_sender = to_mixed( to_upper( ls_msg-role ) ).
              l_target = 'Assistant'.

              ls_msg-content = <ls_anthropic_chat_response>-text.

            WHEN 'tool_result'.

              l_sender = 'Tool'.
              l_target = 'Assistant'.

              ls_msg-content = <ls_anthropic_chat_response>-content.

          ENDCASE.

          me->add_step(
            i_sender  = l_sender
            i_target  = l_target
            i_content = ls_msg-content
          ).

        ENDLOOP.

      ELSE.

        /ui2/cl_json=>deserialize(
          EXPORTING
            json = ls_msg-content
          CHANGING
            data = ls_msg-content
        ).

        l_sender = to_mixed( to_upper( ls_msg-role ) ).
        l_target = 'Assistant'.

        me->add_step(
          i_sender  = l_sender
          i_target  = l_target
          i_content = ls_msg-content
        ).

      ENDIF.

    ELSE.

      FREE lt_anthropic_chat_response.

      /ui2/cl_json=>deserialize(
        EXPORTING
          json = ls_msg-content
        CHANGING
          data = lt_anthropic_chat_response
      ).

      LOOP AT lt_anthropic_chat_response ASSIGNING <ls_anthropic_chat_response>.

        CASE <ls_anthropic_chat_response>-type.

          WHEN 'text'.

            ls_msg-content = <ls_anthropic_chat_response>-text.

            l_sender = to_mixed( to_upper( ls_msg-role ) ).
            l_target = 'User'.

          WHEN 'tool_use'.

            ls_msg-content = <ls_anthropic_chat_response>-name.

            l_sender = to_mixed( to_upper( ls_msg-role ) ).
            l_target = 'Tool'.

        ENDCASE.

        me->add_step(
          i_sender  = l_sender
          i_target  = l_target
          i_content = ls_msg-content
        ).

        ls_msg-content = <ls_anthropic_chat_response>-text.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_diagram_anthropic~get_diagram.

    r_diagram = me->m_diagram.

    IF i_chat_id IS SUPPLIED.

      DATA(lt_messages) = me->get_chat_messages( i_chat_id ).

      LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

        me->add_message( <ls_message> ).

      ENDLOOP.

    ENDIF.

    LOOP AT me->mt_participants ASSIGNING FIELD-SYMBOL(<ls_participant>).

      r_diagram = |{ r_diagram }participant { to_mixed( to_upper( <ls_participant>-participant ) ) }{ cl_abap_char_utilities=>newline }|.

    ENDLOOP.

    LOOP AT me->mt_steps ASSIGNING FIELD-SYMBOL(<ls_step>).

      r_diagram = |{ r_diagram }{ <ls_step>-step }|.

    ENDLOOP.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    out->write( me->get_diagram( '7EA3422BA1AC1FE0AE910DC49DEBCC68' ) ).

  ENDMETHOD.

ENDCLASS.
