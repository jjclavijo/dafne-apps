from pyarrow.ipc import Message as Message, MessageReader as MessageReader, MetadataVersion as MetadataVersion, RecordBatchFileReader as RecordBatchFileReader, RecordBatchFileWriter as RecordBatchFileWriter, RecordBatchStreamReader as RecordBatchStreamReader, RecordBatchStreamWriter as RecordBatchStreamWriter, deserialize_pandas as deserialize_pandas, serialize_pandas as serialize_pandas
from pyarrow.lib import Array as Array, ArrowCapacityError as ArrowCapacityError, ArrowException as ArrowException, ArrowIOError as ArrowIOError, ArrowIndexError as ArrowIndexError, ArrowInvalid as ArrowInvalid, ArrowKeyError as ArrowKeyError, ArrowMemoryError as ArrowMemoryError, ArrowNotImplementedError as ArrowNotImplementedError, ArrowSerializationError as ArrowSerializationError, ArrowTypeError as ArrowTypeError, BaseExtensionType as BaseExtensionType, BinaryArray as BinaryArray, BinaryScalar as BinaryScalar, BooleanArray as BooleanArray, BooleanScalar as BooleanScalar, Buffer as Buffer, BufferOutputStream as BufferOutputStream, BufferReader as BufferReader, BufferedInputStream as BufferedInputStream, BufferedOutputStream as BufferedOutputStream, BuildInfo as BuildInfo, ChunkedArray as ChunkedArray, Codec as Codec, CompressedInputStream as CompressedInputStream, CompressedOutputStream as CompressedOutputStream, DataType as DataType, Date32Array as Date32Array, Date32Scalar as Date32Scalar, Date64Array as Date64Array, Date64Scalar as Date64Scalar, Decimal128Array as Decimal128Array, Decimal128Scalar as Decimal128Scalar, Decimal128Type as Decimal128Type, Decimal256Array as Decimal256Array, Decimal256Scalar as Decimal256Scalar, Decimal256Type as Decimal256Type, DeserializationCallbackError as DeserializationCallbackError, DictionaryArray as DictionaryArray, DictionaryMemo as DictionaryMemo, DictionaryScalar as DictionaryScalar, DictionaryType as DictionaryType, DoubleScalar as DoubleScalar, DurationArray as DurationArray, DurationScalar as DurationScalar, DurationType as DurationType, ExtensionArray as ExtensionArray, ExtensionType as ExtensionType, Field as Field, FixedSizeBinaryArray as FixedSizeBinaryArray, FixedSizeBinaryScalar as FixedSizeBinaryScalar, FixedSizeBinaryType as FixedSizeBinaryType, FixedSizeBufferWriter as FixedSizeBufferWriter, FixedSizeListArray as FixedSizeListArray, FixedSizeListScalar as FixedSizeListScalar, FixedSizeListType as FixedSizeListType, FloatScalar as FloatScalar, FloatingPointArray as FloatingPointArray, HalfFloatScalar as HalfFloatScalar, HdfsFile as HdfsFile, Int16Array as Int16Array, Int16Scalar as Int16Scalar, Int32Array as Int32Array, Int32Scalar as Int32Scalar, Int64Array as Int64Array, Int64Scalar as Int64Scalar, Int8Array as Int8Array, Int8Scalar as Int8Scalar, IntegerArray as IntegerArray, KeyValueMetadata as KeyValueMetadata, LargeBinaryArray as LargeBinaryArray, LargeBinaryScalar as LargeBinaryScalar, LargeListArray as LargeListArray, LargeListScalar as LargeListScalar, LargeListType as LargeListType, LargeStringArray as LargeStringArray, LargeStringScalar as LargeStringScalar, ListArray as ListArray, ListScalar as ListScalar, ListType as ListType, LoggingMemoryPool as LoggingMemoryPool, MapArray as MapArray, MapScalar as MapScalar, MapType as MapType, MemoryMappedFile as MemoryMappedFile, MemoryPool as MemoryPool, MockOutputStream as MockOutputStream, NA as NA, NativeFile as NativeFile, NullArray as NullArray, NullScalar as NullScalar, NumericArray as NumericArray, OSFile as OSFile, ProxyMemoryPool as ProxyMemoryPool, PyExtensionType as PyExtensionType, PythonFile as PythonFile, RecordBatch as RecordBatch, ResizableBuffer as ResizableBuffer, Scalar as Scalar, Schema as Schema, SerializationCallbackError as SerializationCallbackError, SparseCOOTensor as SparseCOOTensor, SparseCSCMatrix as SparseCSCMatrix, SparseCSFTensor as SparseCSFTensor, SparseCSRMatrix as SparseCSRMatrix, StringArray as StringArray, StringScalar as StringScalar, StructArray as StructArray, StructScalar as StructScalar, StructType as StructType, Table as Table, Tensor as Tensor, Time32Array as Time32Array, Time32Scalar as Time32Scalar, Time32Type as Time32Type, Time64Array as Time64Array, Time64Scalar as Time64Scalar, Time64Type as Time64Type, TimestampArray as TimestampArray, TimestampScalar as TimestampScalar, TimestampType as TimestampType, TransformInputStream as TransformInputStream, UInt16Array as UInt16Array, UInt16Scalar as UInt16Scalar, UInt32Array as UInt32Array, UInt32Scalar as UInt32Scalar, UInt64Array as UInt64Array, UInt64Scalar as UInt64Scalar, UInt8Array as UInt8Array, UInt8Scalar as UInt8Scalar, UnionArray as UnionArray, UnionScalar as UnionScalar, UnionType as UnionType, UnknownExtensionType as UnknownExtensionType, VersionInfo as VersionInfo, allocate_buffer as allocate_buffer, array as array, binary as binary, bool_ as bool_, chunked_array as chunked_array, compress as compress, concat_arrays as concat_arrays, concat_tables as concat_tables, cpp_build_info as cpp_build_info, cpp_version as cpp_version, cpp_version_info as cpp_version_info, cpu_count as cpu_count, create_memory_map as create_memory_map, date32 as date32, date64 as date64, decimal128 as decimal128, decimal256 as decimal256, decompress as decompress, default_memory_pool as default_memory_pool, deserialize as deserialize, deserialize_components as deserialize_components, deserialize_from as deserialize_from, dictionary as dictionary, duration as duration, field as field, float16 as float16, float32 as float32, float64 as float64, foreign_buffer as foreign_buffer, from_numpy_dtype as from_numpy_dtype, have_libhdfs as have_libhdfs, infer_type as infer_type, input_stream as input_stream, int16 as int16, int32 as int32, int64 as int64, int8 as int8, jemalloc_memory_pool as jemalloc_memory_pool, jemalloc_set_decay_ms as jemalloc_set_decay_ms, large_binary as large_binary, large_list as large_list, large_string as large_string, large_utf8 as large_utf8, list_ as list_, log_memory_allocations as log_memory_allocations, logging_memory_pool as logging_memory_pool, map_ as map_, memory_map as memory_map, mimalloc_memory_pool as mimalloc_memory_pool, null as null, nulls as nulls, output_stream as output_stream, proxy_memory_pool as proxy_memory_pool, py_buffer as py_buffer, read_serialized as read_serialized, record_batch as record_batch, register_extension_type as register_extension_type, repeat as repeat, scalar as scalar, schema as schema, serialize as serialize, serialize_to as serialize_to, set_cpu_count as set_cpu_count, set_memory_pool as set_memory_pool, string as string, struct as struct, system_memory_pool as system_memory_pool, table as table, time32 as time32, time64 as time64, timestamp as timestamp, total_allocated_bytes as total_allocated_bytes, transcoding_input_stream as transcoding_input_stream, type_for_alias as type_for_alias, uint16 as uint16, uint32 as uint32, uint64 as uint64, uint8 as uint8, unify_schemas as unify_schemas, union as union, unregister_extension_type as unregister_extension_type, utf8 as utf8
from pyarrow.serialization import default_serialization_context as default_serialization_context, register_default_serialization_handlers as register_default_serialization_handlers, register_torch_serialization_handlers as register_torch_serialization_handlers
from typing import Any

def parse_git(root: Any, **kwargs: Any): ...
def show_versions() -> None: ...
def __getattr__(name: Any): ...

localfs: Any
FileSystem: Any
LocalFileSystem: Any
HadoopFileSystem: Any
SerializationContext: Any
SerializedPyObject: Any
read_message: Any
read_record_batch: Any
read_schema: Any
read_tensor: Any
write_tensor: Any
get_record_batch_size: Any
get_tensor_size: Any
open_stream: Any
open_file: Any
ArrayValue: Any
NullType: Any
BooleanValue: Any
Int8Value: Any
Int16Value: Any
Int32Value: Any
Int64Value: Any
UInt8Value: Any
UInt16Value: Any
UInt32Value: Any
UInt64Value: Any
HalfFloatValue: Any
FloatValue: Any
DoubleValue: Any
ListValue: Any
LargeListValue: Any
MapValue: Any
FixedSizeListValue: Any
BinaryValue: Any
StringValue: Any
LargeBinaryValue: Any
LargeStringValue: Any
FixedSizeBinaryValue: Any
Decimal128Value: Any
Decimal256Value: Any
UnionValue: Any
StructValue: Any
DictionaryValue: Any
Date32Value: Any
Date64Value: Any
Time32Value: Any
Time64Value: Any
TimestampValue: Any
DurationValue: Any

def get_include(): ...
def get_libraries(): ...
def create_library_symlinks(): ...
def get_library_dirs(): ...
