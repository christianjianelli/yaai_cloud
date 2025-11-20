CLASS ycl_aaic_basic_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_AAIC_BASIC_SETUP IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    INSERT yaaic_api FROM TABLE @( VALUE #( ( id = 'OPENAI' base_url = 'https://api.openai.com' )
                                            ( id = 'ANTHROPIC' base_url = 'https://api.anthropic.com' )
                                            ( id = 'GOOGLE' base_url = 'https://generativelanguage.googleapis.com' )
                                            ( id = 'MISTRAL' base_url = 'https://api.mistral.ai' ) ) ) ACCEPTING DUPLICATE KEYS.

    INSERT yaaic_model FROM TABLE @( VALUE #( ( id = 'OPENAI' model = 'gpt-5' default_model = abap_true )
                                              ( id = 'ANTHROPIC' model = 'claude-sonnet-4-20250514' default_model = abap_true )
                                              ( id = 'GOOGLE' model = 'gemini-2.5-flash' default_model = abap_true )
                                              ( id = 'MISTRAL' model = 'mistral-large-latest' default_model = abap_true ) ) ) ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.
ENDCLASS.
