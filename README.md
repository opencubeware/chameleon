# Introduction

**This project is not even beta. You have been warned ;).**  
It's being developed together with our other projects' needs, however you may and should feel free to contribute to this project.

The goal of this project is to keep logic that transforms external data format (currently JSON only but we use Erlang proplists in some places as an external format as well) to and from Erlang terms (mainly records as they are terms that are kept in Mnesia) in one place.

chameleon is using mochijson2 for JSON encoding/decoding.

# Transformations

In order to use chameleon you should start the chameleon application, e.g. by `ok = application:start(chameleon).`

## Proplists

To convert an Erlang proplist to JSON iolist simply call `{ok, Json} = chameleon:json(Proplist)`.  

To convert a JSON binary to Erlang proplist call `{ok, Proplist} = chameleon:proplist(Json)`.  
Please note that returned proplist keys will be binaries, not atoms as one could expect. This behaviour will most likely change in the future.

## Records

### Record information extraction
chameleon needs record information to be extracted in order to perform records transformations.  

This should be done by adding `-compile({parse_transform, chameleon_transform}).` flag to the module that you want to extract records from and calling `chameleon:records(Module)`.  

If your application is an OTP application it might be a good idea to add mentioned flag to the application behaviour module and call `chameleon:records` when application starts.

### Nested records
chameleon supports nested records, however you have to provide the name of dependent record in the record definition.

Investigate the following example:
```
-record(company,    {name,
                     city,
                     country}).
-record(department, {name,
                     company :: #company{}}).
```

### Default values
chameleon respects default field values provided in record definitions

### Usage

If you want to convert JSON binary or proplist to record call `{ok, Record} = chameleon:record(JsonOrProplist)`.  
Please note that either JSON or proplist must have root element which name matches record name.  
Fields that don't exist in the record definition will be ignored.

To convert record to proplist call `{ok, Proplist} = chameleon:proplist(Record)`.  
To convert record to JSON iolist call `{ok, Json} = chameleon:json(Record)`.


## Lists (of proplists and records)
Preceding conversions **should** work fine if you provide a list of proplists/records or a JSON list of objects that would be transformed to proplists or records if provided alone.

# Filters

Soon... :)

# Tests

You can find a Common Test suite in **test** directory.  
If you are contributing to this project you are asked to cover the new functionality with some tests.