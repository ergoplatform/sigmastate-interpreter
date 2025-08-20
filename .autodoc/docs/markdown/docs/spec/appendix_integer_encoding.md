[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/docs/spec/appendix_integer_encoding.tex)

This file contains two methods for encoding integer values in a compressed format. The first method is called VLQ encoding, which stands for Variable Length Quantity encoding. This method takes a long integer value as input and encodes it into a sequence of bytes that can be efficiently stored in memory. The encoded value is stored in a byte buffer, which is a fixed-size array of bytes.

The encoding process works by breaking the input value into 7-bit chunks and storing each chunk in a separate byte. The most significant bit of each byte is set to 1 to indicate that there are more bytes to follow. The least significant byte has its most significant bit set to 0 to indicate that it is the last byte in the sequence. This ensures that the encoded value can be reconstructed correctly by reading the bytes in the correct order.

The second method is called ZigZag encoding, which is used to encode signed integers into values that can be efficiently encoded with VLQ encoding. This method takes a signed 64-bit integer as input and returns an unsigned 64-bit integer, stored in a signed int because Java has no explicit unsigned support.

The encoding process works by first left-shifting the input value by 1 bit and then performing a bitwise XOR operation with the right-shifted input value by 63 bits. This converts the signed integer into an unsigned integer that can be encoded using VLQ encoding.

These encoding methods are useful for compressing large integer values that need to be stored or transmitted efficiently. They can be used in a variety of applications, such as data compression, network protocols, and file formats. For example, they could be used to compress large datasets in a database or to encode metadata in a file format. Here is an example of how to use the VLQ encoding method:

```
byte[] buffer = new byte[10];
int position = 0;
long value = 1234567890L;
putULong(value);
```

This code creates a byte buffer of size 10 and initializes the position to 0. It then encodes the long integer value using the putULong method and stores the encoded bytes in the buffer. The encoded value can be retrieved by reading the bytes from the buffer in the correct order and decoding them using the reverse process.
## Questions: 
 1. What is the purpose of the \texttt{putULong} method?
   
   The \texttt{putULong} method is used for compressed encoding of integer values using variable-length quantity (VLQ) encoding.

2. What is ZigZag encoding and why is it used?
   
   ZigZag encoding is a method of encoding signed integers into values that can be efficiently encoded with varint. It is used to avoid sign-extension of negative values to 64 bits, which would always take 10 bytes in the buffer.

3. Why is the returned value of \texttt{encodeZigZag64} stored in a signed int instead of an unsigned long?
   
   The returned value of \texttt{encodeZigZag64} is stored in a signed int because Java has no explicit support for unsigned types.