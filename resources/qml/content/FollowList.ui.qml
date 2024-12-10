import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import HsQML.Model 1.0
import Futr 1.0

Rectangle {
    width: parent.width * 0.3 - (parent.spacing * 2 / 3)
    height: parent.height
    color: Material.backgroundColor
    radius: 5

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 10
        spacing: 10

        FollowListFilter {
            id: followListFilter
        }

        // Follows list
        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            color: "transparent"

            ListView {
                id: followsView
                anchors.fill: parent
                clip: true
                spacing: 5
                property string selectedPubkey: ""

                model: AutoListModel {
                    id: followsModel
                    source: followList
                    mode: AutoListModel.ByKey
                    equalityTest: function (oldItem, newItem) {
                        return oldItem.pubkey === newItem.pubkey
                            && oldItem.relay === newItem.relay
                            && oldItem.petname === newItem.petname
                            && oldItem.displayName === newItem.displayName
                            && oldItem.name === newItem.name
                            && oldItem.picture === newItem.picture
                    }
                }

                highlightRangeMode: ListView.ApplyRange
                highlightMoveDuration: 0
                cacheBuffer: height * 0.5
                displayMarginBeginning: height * 0.5
                displayMarginEnd: height * 0.5
                reuseItems: true

                ScrollBar.vertical: ScrollBar {
                    active: true
                    policy: ScrollBar.AlwaysOn
                }

                delegate: Rectangle {
                    id: followItem
                    property bool mouseHover: false
                    height: visible ? 80 : 0
                    width: followsView.width - followsView.ScrollBar.vertical.width
                    visible: {
                        if (!modelData) return false;
                        if (followListFilter.filterText === "") return true;
                        var searchText = followListFilter.filterText.toLowerCase();
                        return modelData.pubkey.toLowerCase().includes(searchText) ||
                                (modelData.displayName && modelData.displayName.toLowerCase().includes(searchText));
                    }
                    color: {
                        if (!modelData) return Material.backgroundColor;
                        if (mouseHover) return Material.accentColor;
                        if (modelData.pubkey === followsView.selectedPubkey) return Qt.darker(Material.accentColor, 1.2);
                        return Material.backgroundColor;
                    }
                    border.color: Material.dividerColor
                    radius: 5

                    RowLayout {
                        anchors.fill: parent
                        anchors.margins: 10

                        Image {
                            source: Util.getProfilePicture(modelData.picture, modelData.pubkey)
                            Layout.preferredWidth: 50
                            Layout.preferredHeight: 50
                            Layout.alignment: Qt.AlignVCenter
                            smooth: true
                            fillMode: Image.PreserveAspectCrop
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 5

                            Text {
                                text: modelData.petname ||modelData.displayName || modelData.name || modelData.pubkey
                                font: Constants.font
                                color: Material.primaryTextColor
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                            }

                            Text {
                                text: modelData.name || modelData.pubkey
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                font: Constants.smallFont
                                color: Material.secondaryTextColor
                                visible: modelData.displayName !== "" || modelData.name !== ""
                            }
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true

                        onEntered: followItem.mouseHover = true
                        onExited: followItem.mouseHover = false

                        onClicked: {
                            followsView.selectedPubkey = modelData.pubkey
                            setCurrentProfile(modelData.pubkey)
                            openChat(modelData.pubkey)
                            profileLoader.setSource("Profile/Profile.ui.qml", {
                                "profileData": currentProfile,
                                "npub": modelData.pubkey
                            })
                            chatLoader.setSource("MainContent.ui.qml", {
                                "profileData": currentProfile,
                                "npub": modelData.pubkey
                            })
                        }
                    }
                }
            }
        }
    }
}
