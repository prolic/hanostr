pragma Singleton
import QtQuick 2.15

QtObject {
    function getProfilePicture(picture, npub) {
        return picture ? picture : "https://robohash.org/" + npub + ".png?size=50x50";
    }

    function getFormattedContent(post) {
        if (!post) return ""
        
        let content = ""
        try {
            if (post.postType === "quote_repost") {
                content = (post.content || "").replace(/nostr:(note|nevent|naddr)1[a-zA-Z0-9]+/g, '').trim()
            } else if (post.postType === "repost") {
                content = post.referencedContent || ""
            } else {
                content = post.content || ""
            }
        } catch (e) {
            console.error("Error formatting content:", e)
            content = ""
        }
        return content
    }
}
