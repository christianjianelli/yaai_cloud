INTERFACE yif_aaic_prompt_template
  PUBLIC.

  DATA: m_template_id   TYPE c LENGTH 30,
        m_template_text TYPE string READ-ONLY.

  METHODS set_template_text
    IMPORTING
      i_template_text TYPE clike.

  METHODS get_template_text
    RETURNING VALUE(r_template_text) TYPE string.

ENDINTERFACE.
