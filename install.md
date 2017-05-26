1. put `okuno-info.service` into `/etc/systemd/system/`

2. put `okuno-info` binary and `okuno-info.sh`, `config/`, `static/` into `/var/lib/okuno-info`

3. make sure there's an `okuno` user, and that everything in `/var/lib/okuno-info` is owned by it

4. `systemctl enable okuno-info.service`

5. reboot, or `sudo service okuno-info start`
