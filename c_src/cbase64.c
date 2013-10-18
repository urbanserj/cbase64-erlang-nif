/* ex: ts=4 sw=4 et
 *
 * Copyright (c) 2011 Sergey Urbanovich
 * http://github.com/urbanserj/cbase64-erlang-nif
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "erl_nif.h"

#define min(X, Y)  ((X) < (Y) ? (X) : (Y))

#if !(ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 4)
int enif_consume_timeslice(ErlNifEnv* env, int percent) {
    return 0;
}
#endif

#define TIMESLICE 10
#define SPEEDUP 30
#define REDUCTIONS 2000
#define ITER (REDUCTIONS * SPEEDUP / TIMESLICE)


static const char cb64[]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

inline void encodeblock(const u_char in[3], u_char out[4], size_t len)
{
    out[0] = cb64[ (in[0] >> 2) & 0x3f ];
    out[1] = cb64[ ((in[0] << 4) + (--len ? in[1] >> 4 : 0)) & 0x3f ];
    out[2] = len ? cb64[ ((in[1] << 2) + (--len ? in[2] >> 6 : 0)) & 0x3f ] : '=';
    out[3] = len ? cb64[ in[2] & 0x3f ] : '=';
}

static ERL_NIF_TERM encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    ErlNifBinary buf;
    u_char *buf_data;
    int buf_size;
    ERL_NIF_TERM ret, data_ret;
    size_t it, it_b64;
    size_t timeslice;

    if ( enif_inspect_binary(env, argv[0], &data) ) {
        data_ret = argv[0];
    } else if ( enif_inspect_iolist_as_binary(env, argv[0], &data) ) {
        data_ret = enif_make_binary(env, &data);
    } else {
        return enif_make_badarg(env);
    }

    if ( enif_inspect_binary(env, argv[1], &buf) ) {
        ret = argv[1];
        buf_data = buf.data;
    } else {
        size_t buf_size = ( data.size + 2 ) / 3 * 4;
        buf_data = enif_make_new_binary(env, buf_size, &ret);
    }

    if ( !enif_get_int(env, argv[2], &buf_size) )
        return enif_make_badarg(env);

    it = buf_size / 4 * 3;
    it_b64 = buf_size;
    do {
        for (; it < data.size && it_b64 < buf_size + ITER * 4; it += 3, it_b64 += 4)
            encodeblock(data.data + it, buf_data + it_b64, min(data.size-it, 3));
        timeslice = 10 * (it_b64 - buf_size) / 4 / ITER;
        buf_size = it_b64;
    } while ( !enif_consume_timeslice(env, timeslice) && (it + 3 < data.size));

    if (it + 3 < data.size)
        return enif_make_tuple3(env, data_ret, ret, enif_make_int(env, buf_size));
    return ret;
}


#define DECODE_ERROR 0xffffffff

#define B64(_) \
    ( (_) >= 'A' && (_) <= 'Z' ? 25 - 'Z' + (_) : \
      (_) >= 'a' && (_) <= 'z' ? 51 - 'z' + (_) : \
      (_) >= '0' && (_) <= '9' ? 61 - '9' + (_) : \
      (_) == '+' ? 62 : (_) ? 63 : DECODE_ERROR )

static const int cd64[256] = {
    B64(  0), B64(  1), B64(  2), B64(  3), B64(  4), B64(  5), B64(  6), B64(  7),
    B64(  8), B64(  9), B64( 10), B64( 11), B64( 12), B64( 13), B64( 14), B64( 15),
    B64( 16), B64( 17), B64( 18), B64( 19), B64( 20), B64( 21), B64( 22), B64( 23),
    B64( 24), B64( 25), B64( 26), B64( 27), B64( 28), B64( 29), B64( 30), B64( 31),
    B64( 32), B64( 33), B64( 34), B64( 35), B64( 36), B64( 37), B64( 38), B64( 39),
    B64( 40), B64( 41), B64( 42), B64( 43), B64( 44), B64( 45), B64( 46), B64( 47),
    B64( 48), B64( 49), B64( 50), B64( 51), B64( 52), B64( 53), B64( 54), B64( 55),
    B64( 56), B64( 57), B64( 58), B64( 59), B64( 60), B64( 61), B64( 62), B64( 63),
    B64( 64), B64( 65), B64( 66), B64( 67), B64( 68), B64( 69), B64( 70), B64( 71),
    B64( 72), B64( 73), B64( 74), B64( 75), B64( 76), B64( 77), B64( 78), B64( 79),
    B64( 80), B64( 81), B64( 82), B64( 83), B64( 84), B64( 85), B64( 86), B64( 87),
    B64( 88), B64( 89), B64( 90), B64( 91), B64( 92), B64( 93), B64( 94), B64( 95),
    B64( 96), B64( 97), B64( 98), B64( 99), B64(100), B64(101), B64(102), B64(103),
    B64(104), B64(105), B64(106), B64(107), B64(108), B64(109), B64(110), B64(111),
    B64(112), B64(113), B64(114), B64(115), B64(116), B64(117), B64(118), B64(119),
    B64(120), B64(121), B64(122), B64(123), B64(124), B64(125), B64(126), B64(127),
    B64(128), B64(129), B64(130), B64(131), B64(132), B64(133), B64(134), B64(135),
    B64(136), B64(137), B64(138), B64(139), B64(140), B64(141), B64(142), B64(143),
    B64(144), B64(145), B64(146), B64(147), B64(148), B64(149), B64(150), B64(151),
    B64(152), B64(153), B64(154), B64(155), B64(156), B64(157), B64(158), B64(159),
    B64(160), B64(161), B64(162), B64(163), B64(164), B64(165), B64(166), B64(167),
    B64(168), B64(169), B64(170), B64(171), B64(172), B64(173), B64(174), B64(175),
    B64(176), B64(177), B64(178), B64(179), B64(180), B64(181), B64(182), B64(183),
    B64(184), B64(185), B64(186), B64(187), B64(188), B64(189), B64(190), B64(191),
    B64(192), B64(193), B64(194), B64(195), B64(196), B64(197), B64(198), B64(199),
    B64(200), B64(201), B64(202), B64(203), B64(204), B64(205), B64(206), B64(207),
    B64(208), B64(209), B64(210), B64(211), B64(212), B64(213), B64(214), B64(215),
    B64(216), B64(217), B64(218), B64(219), B64(220), B64(221), B64(222), B64(223),
    B64(224), B64(225), B64(226), B64(227), B64(228), B64(229), B64(230), B64(231),
    B64(232), B64(233), B64(234), B64(235), B64(236), B64(237), B64(238), B64(239),
    B64(240), B64(241), B64(242), B64(243), B64(244), B64(245), B64(246), B64(247),
    B64(248), B64(249), B64(250), B64(251), B64(252), B64(253), B64(254), B64(255)
};

inline int decodeblock(const u_char in[4], u_char out[3])
{
    int code;
    size_t it = 0;
    unsigned int val = 0;

    for (;it < 4; ++it) {
        if ( (code = cd64[in[it]]) == DECODE_ERROR )
            return DECODE_ERROR;
        val = (val << 6) + code;
    }

    out[0] = (val >> 16) & 0xff;
    out[1] = (val >> 8) & 0xff;
    out[2] = val & 0xff;

    return 0;
}

inline int decodeblock_tail(const u_char in[4], u_char out[3])
{
    int code;
    size_t it = 0;
    unsigned int val = 0;

    if ( in[0] == '=' || in[1] == '=' ||
        (in[2] == '=' && in[3] != '=') )
        return DECODE_ERROR;

    for (;it < 4; ++it) {
        val <<= 6;
        if ( in[it] == '=' )
            continue;
        if ( (code = cd64[in[it]]) == DECODE_ERROR )
            return DECODE_ERROR;
        val += code;
    }

    out[0] = (val >> 16) & 0xff;
    if ( in[2] != '=' )
        out[1] = (val >> 8) & 0xff;
    if ( in[3] != '=' )
        out[2] = val & 0xff;

    return 0;
}

static ERL_NIF_TERM decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    ErlNifBinary data;
    ErlNifBinary buf;
    u_char *buf_data;
    int buf_size;
    ERL_NIF_TERM ret, data_ret;
    size_t it, it_b64;
    size_t timeslice;

    if ( enif_inspect_binary(env, argv[0], &data) ) {
        data_ret = argv[0];
    } else if ( enif_inspect_iolist_as_binary(env, argv[0], &data) ) {
        data_ret = enif_make_binary(env, &data);
    } else {
        return enif_make_badarg(env);
    }

    if ( data.size == 0 )
        return argv[0];

    if ( enif_inspect_binary(env, argv[1], &buf) ) {
        ret = argv[1];
        buf_data = buf.data;
    } else {
        size_t buf_size = data.size/4*3
            - (data.data[data.size - 1] == '=' ? 1 : 0)
            - (data.data[data.size - 2] == '=' ? 1 : 0);
        buf_data = enif_make_new_binary(env, buf_size, &ret);
    }

    if ( !enif_get_int(env, argv[2], &buf_size) )
        return enif_make_badarg(env);

    it = buf_size;
    it_b64 = buf_size / 3 * 4;

    do {
        for (; it_b64 < data.size - 4 && it < buf_size + ITER * 4; it += 3, it_b64 += 4)
            if ( decodeblock(data.data + it_b64, buf_data + it) )
                goto BADARG;
        timeslice = 10 * (it - buf_size) / 4 / ITER;
        buf_size = it;
    } while ( !enif_consume_timeslice(env, timeslice) && (it_b64 + 4 < data.size));
    if (it_b64 + 4 < data.size) {
        return enif_make_tuple3(env, data_ret, ret, enif_make_int(env, buf_size));
    }
    if ( decodeblock_tail(data.data + it_b64, buf_data + it) )
        goto BADARG;
    return ret;

BADARG:
    enif_release_binary(&buf);
    return enif_make_badarg(env);
}



static ErlNifFunc nif_funcs[] =
{
    {"nif_encode", 3, encode},
    {"nif_decode", 3, decode}
};
ERL_NIF_INIT(cbase64, nif_funcs, NULL, NULL, NULL, NULL)
