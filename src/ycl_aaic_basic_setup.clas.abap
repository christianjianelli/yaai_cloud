CLASS ycl_aaic_basic_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_basic_setup IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    INSERT yaaic_base_url FROM TABLE @( VALUE #( ( id = 'OPENAI' base_url = 'https://api.openai.com' )
                                                 ( id = 'ANTHROPIC' base_url = 'https://api.anthropic.com' )
                                                 ( id = 'GOOGLE' base_url = 'https://generativelanguage.googleapis.com' )
                                                 ( id = 'MISTRAL' base_url = 'https://api.mistral.ai' ) ) ) ACCEPTING DUPLICATE KEYS.

    INSERT yaaic_model FROM TABLE @( VALUE #( ( id = 'OPENAI' model = 'gpt-5' )
                                              ( id = 'ANTHROPIC' model = 'claude-sonnet-4-20250514' )
                                              ( id = 'GOOGLE' model = 'gemini-2.5-flash' )
                                              ( id = 'MISTRAL' model = 'mistral-large-latest' ) ) ) ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.

ENDCLASS.
