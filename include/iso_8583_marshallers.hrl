-define(MARSHALLER_ASCII, [{field_marshaller, erl8583_marshaller_ascii}, 
						   {bitmap_marshaller, erl8583_marshaller_ascii},
						   {mti_marshaller, erl8583_marshaller_ascii},
						   {end_marshaller, erl8583_marshaller_ascii}]).

-define(MARSHALLER_BINARY, [{field_marshaller, erl8583_marshaller_binary}, 
						    {bitmap_marshaller, erl8583_marshaller_binary},
						    {mti_marshaller, erl8583_marshaller_binary},
						    {end_marshaller, erl8583_marshaller_binary}]).


-define(MARSHALLER_EBCDIC, [{field_marshaller, erl8583_marshaller_ebcdic},
							{bitmap_marshaller, erl8583_marshaller_ebcdic},
							{mti_marshaller, erl8583_marshaller_ebcdic},
							{end_marshaller, erl8583_marshaller_ebcdic}]).

-define(MARSHALLER_JSON, [{field_marshaller, erl8583_marshaller_json}, 
						  {bitmap_marshaller, erl8583_marshaller_json},
						  {mti_marshaller, erl8583_marshaller_json},
						  {init_marshaller, erl8583_marshaller_json},
						  {end_marshaller, erl8583_marshaller_json}]).

-define(MARSHALLER_XML, [{field_marshaller, erl8583_marshaller_xml},
						 {bitmap_marshaller, erl8583_marshaller_xml},
						 {mti_marshaller, erl8583_marshaller_xml},
						 {init_marshaller, erl8583_marshaller_xml},
						 {end_marshaller, erl8583_marshaller_xml}]).

-define(MARSHALLER_GRPC, [{field_marshaller, iso_8583_marshaller_grpc},
						  {bitmap_marshaller, iso_8583_marshaller_grpc},
						  {mti_marshaller, iso_8583_marshaller_grpc},
						  {message_marshaller, iso_8583_marshaller_grpc}]).