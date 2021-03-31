# void-service-manager

Manage services within VoidLinux via UNIX style command line interface.

Although this really just uses `runit` under the hood, implementation is scoped for VoidLinux only.

## Installation

### xbps

```sh
xbps-install vsm
```

### void-package-manager

```sh
vpm vsm
```

## Usage

todo - look up the void docs for the required jargon

```sh
# Manage a service
vpm -{f,i,o} -{e|d|t} <service>

# List a service
vpm -{f,i,o} -{e|d,t} 
```

### Managing Services

Services can be enabled, disabled or tested.

#### Enable



#### Disable



#### Test

Tested services:
- Don't turn on when machine boots
- Don't restart the service when it stops


### Listing Services

Services can be filtered for enable, disabled and test services.
Enabled and disabled are mutually exclusive