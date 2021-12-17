import dataclasses
import enum
import operator
from functools import reduce
import typing


class PacketType(enum.Enum):
    """Enumeration for packet type.
    """
    SUM = 0
    PRODUCT = 1
    MIN = 2
    MAX = 3
    LITERAL = 4
    GREATER_THAN = 5
    LESS_THAN = 6
    EQUAL_TO = 7


@dataclasses.dataclass
class Packet:
    version: int
    type: PacketType
    subpackets: typing.Optional[typing.List['Packet']]
    literal: typing.Optional[int]


def bits_to_int(bits: typing.List[bool]) -> int:
    """Converts big-endian list of bits to an integer.
    """
    return reduce(lambda state, new: 2 * state + new, bits, 0)


def parse_packet(bits: typing.List[bool]) -> typing.Tuple[Packet, int]:
    """Recursively parses a packet given a list of bits to read from.

    Args:
        bits (typing.List[bool]): List of bits to read packet from.

    Returns:
        typing.Tuple[Packet, int]: Tuple of (parsed packet, resumption index)
                                   Where the "resumption index" indicates where
                                   future parsing should resume reading.
    """

    packet_version = bits_to_int(bits[:3])
    packet_type = PacketType(bits_to_int(bits[3:6]))

    # create packet object
    packet = Packet(version=packet_version, type=packet_type,
                    subpackets=None, literal=None)

    if packet_type == PacketType.LITERAL:
        # read literal value
        literal_bits = []  # nibble = 4 bits

        # starting from bit at idx 6
        current_index = 6
        while bits[current_index] == 1:
            # 1 bit = "keep reading"
            literal_bits += bits[current_index+1:current_index+5]
            # advance index by 5 locations
            current_index += 5

        # bits[current_index] is zero
        # last nibble started with a zero
        literal_bits += bits[current_index+1:current_index+5]

        # convert to decimal number
        packet.literal = bits_to_int(literal_bits)

        # return completed packet
        # we should pick up reading after the last nibble
        return packet, (current_index + 5)
    else:
        # some kind of operator packet
        # read length type id bit
        length_type_id = bits[6]

        # initialize subpackets to empty list
        packet.subpackets = []

        if length_type_id == 0:
            # next 15 bits represent total length in bits
            total_subpacket_length = bits_to_int(bits[7:7+15])

            subpacket_idx = 7 + 15  # how many bits into the string does this subpacket begin?

            while subpacket_idx < (7 + 15 + total_subpacket_length):
                subpacket, resume = parse_packet(
                    bits[subpacket_idx:])

                # make sure to account for the offset
                subpacket_idx = subpacket_idx + resume

                packet.subpackets.append(subpacket)

            # return the completed packet
            return packet, subpacket_idx
        else:
            # next 11 bits represent the number of subpackets
            num_subpackets = bits_to_int(bits[7:7+11])

            num_read = 0  # number of packets read
            idx = 7+11  # index where we start reading packets

            while num_read < num_subpackets:
                subpacket, resume = parse_packet(bits[idx:])
                # account for the fact that we started at idx
                idx = idx + resume
                packet.subpackets.append(subpacket)
                num_read += 1

            return packet, idx


def hex_to_bit_array(hex_string: str):
    bit_array = []

    for ch in hex_string:
        int_val = int(ch, base=16)
        bit_array += [(True if ch == '1' else False)
                      for ch in "{:04b}".format(int_val)]

    return bit_array


def sum_packet_versions(packet):
    if packet.subpackets is not None:
        return packet.version + sum(map(sum_packet_versions, packet.subpackets))
    else:
        return packet.version


def evaluate(packet):
    if packet.type == PacketType.LITERAL:
        return packet.literal
    elif packet.type == PacketType.SUM:
        return sum(evaluate(p) for p in packet.subpackets)
    elif packet.type == PacketType.PRODUCT:
        return reduce(operator.mul, (evaluate(p) for p in packet.subpackets), 1)
    elif packet.type == PacketType.MIN:
        return min(evaluate(p) for p in packet.subpackets)
    elif packet.type == PacketType.MAX:
        return max(evaluate(p) for p in packet.subpackets)
    elif packet.type == PacketType.GREATER_THAN:
        return int(evaluate(packet.subpackets[0]) > evaluate(packet.subpackets[1]))
    elif packet.type == PacketType.LESS_THAN:
        return int(evaluate(packet.subpackets[0]) < evaluate(packet.subpackets[1]))
    elif packet.type == PacketType.EQUAL_TO:
        return int(evaluate(packet.subpackets[0]) == evaluate(packet.subpackets[1]))


if __name__ == "__main__":
    hex_string = input()
    packet_bits = hex_to_bit_array(hex_string)
    packet, _ = parse_packet(packet_bits)
    print(evaluate(packet))
