INTERFACE yif_aaic_diagram_openai
  PUBLIC.

  TYPES: BEGIN OF ty_participant_s,
           participant TYPE string,
         END OF ty_participant_s,

         BEGIN OF ty_step_s,
           step TYPE string,
         END OF ty_step_s,

         BEGIN OF ty_replacement_s,
           from_char TYPE c LENGTH 1,
           to_string TYPE string,
         END OF ty_replacement_s.

  DATA: mt_participants TYPE STANDARD TABLE OF ty_participant_s,
        mt_steps        TYPE STANDARD TABLE OF ty_step_s,
        mt_replacements TYPE STANDARD TABLE OF ty_replacement_s,
        m_diagram       TYPE string,
        m_maxlen        TYPE i.

  METHODS:

    get_chat_messages
      IMPORTING
                i_chat_id           TYPE csequence
      RETURNING VALUE(r_t_messages) TYPE yif_aaic_openai=>ty_generate_messages_t,

    get_diagram
      IMPORTING
                i_chat_id        TYPE csequence OPTIONAL
      RETURNING VALUE(r_diagram) TYPE string,

    add_participant
      IMPORTING
        i_participant TYPE string,

    add_step
      IMPORTING
        i_sender  TYPE string
        i_target  TYPE string
        i_content TYPE string,

    add_message
      IMPORTING
        i_s_msg TYPE yif_aaic_openai=>ty_generate_message_s,

    parse_json
      IMPORTING
        i_json  TYPE /ui2/cl_json=>json
      EXPORTING
        e_s_msg TYPE yif_aaic_openai=>ty_generate_message_s,

    escape_text
      IMPORTING
        i_text                TYPE string
      RETURNING
        VALUE(r_escaped_text) TYPE string.

ENDINTERFACE.
