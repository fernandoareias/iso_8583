-define(MARSHALLER_ASCII, [{field_marshaller, iso_8583_marshaller_ascii},
						   {bitmap_marshaller, iso_8583_marshaller_ascii},
						   {mti_marshaller, iso_8583_marshaller_ascii},
						   {end_marshaller, iso_8583_marshaller_ascii}]).

-define(MARSHALLER_BINARY, [{field_marshaller, iso_8583_marshaller_binary},
						    {bitmap_marshaller, iso_8583_marshaller_binary},
						    {mti_marshaller, iso_8583_marshaller_binary},
						    {end_marshaller, iso_8583_marshaller_binary}]).


-define(MARSHALLER_EBCDIC, [{field_marshaller, iso_8583_marshaller_ebcdic},
							{bitmap_marshaller, iso_8583_marshaller_ebcdic},
							{mti_marshaller, iso_8583_marshaller_ebcdic},
							{end_marshaller, iso_8583_marshaller_ebcdic}]).

-define(MARSHALLER_JSON, [{field_marshaller, iso_8583_marshaller_json},
						  {bitmap_marshaller, iso_8583_marshaller_json},
						  {mti_marshaller, iso_8583_marshaller_json},
						  {init_marshaller, iso_8583_marshaller_json},
						  {end_marshaller, iso_8583_marshaller_json}]).

-define(MARSHALLER_XML, [{field_marshaller, iso_8583_marshaller_xml},
						 {bitmap_marshaller, iso_8583_marshaller_xml},
						 {mti_marshaller, iso_8583_marshaller_xml},
						 {init_marshaller, iso_8583_marshaller_xml},
						 {end_marshaller, iso_8583_marshaller_xml}]).

-define(MARSHALLER_GRPC, [{field_marshaller, iso_8583_marshaller_grpc},
						  {bitmap_marshaller, iso_8583_marshaller_grpc},
						  {mti_marshaller, iso_8583_marshaller_grpc},
						  {message_marshaller, iso_8583_marshaller_grpc}]).
