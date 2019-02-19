include mk/Erlangbin.mk
include mk/Docker.mk

USER = aialferov

PORT=8088
TARGET=jose

RUN_ARGS = run

DOCKER_RUN_ARGS_EXTRA = \
	--link $(TARGET) \
	--env HOST=$(TARGET) \
	-p $(PORT):$(PORT)

ifdef ERLANG_VERSION
    DOCKER_BUILD_ARGS_EXTRA = \
        --build-arg ERLANG_VERSION=$(ERLANG_VERSION)
endif
